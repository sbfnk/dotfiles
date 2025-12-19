;;; tools/vulpea/autoload.el -*- lexical-binding: t; -*-
;;;
;;;###autoload
(defun vulpea-agenda-category (&optional len)
  "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (vulpea-buffer-prop-get "title"))
         (category (org-get-category))
         (result
          (or (if (and
                   title
                   (string-equal category file-name))
                  title
                category)
              "")))
    (if (numberp len)
        (s-truncate len (s-pad-right len " " result))
      result)))

(defun vulpea-ensure-filetag ()
  "Add respective file tag if it's missing in the current note."
  (interactive)
  (let ((tags (vulpea-buffer-tags-get))
        (tag (vulpea--title-as-tag)))
    (when (and (seq-contains-p tags "people")
               (not (seq-contains-p tags tag)))
      (vulpea-buffer-tags-add tag))))

(defun vulpea--title-as-tag ()
  "Return title of the current note as tag."
  (vulpea--title-to-tag (vulpea-buffer-title-get)))

(defun vulpea--title-to-tag (title)
  "Convert TITLE to tag."
  (concat "@" (s-replace " " "" title)))

(defun vulpea-tags-add ()
  "Add a tag to current note."
  (interactive)
  ;; since https://github.com/org-roam/org-roam/pull/1515
  ;; `org-roam-tag-add' returns added tag, we could avoid reading tags
  ;; in `vulpea-ensure-filetag', but this way it can be used in
  ;; different contexts while having simple implementation.
  (when (call-interactively #'org-roam-tag-add)
    (vulpea-ensure-filetag)))

(defun my-vulpea-insert-handle (note)
  "Hook to be called on NOTE after `vulpea-insert'."
  (when-let* ((title (vulpea-note-title note))
              (tags (vulpea-note-tags note)))
    (when (seq-contains-p tags "people")
      (save-excursion
        (ignore-errors
          (org-back-to-heading)
          (when (eq 'todo (org-element-property
                           :todo-type
                           (org-element-at-point)))
            (org-set-tags
             (seq-uniq
              (cons
               (vulpea--title-to-tag title)
               (org-get-tags nil t))))))))))

(defun vulpea--title-to-tag (title)
  "Convert TITLE to tag."
  (concat "@" (s-replace " " "" title)))

(defun vulpea-project-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (seq-find                                 ; (3)
   (lambda (type)
     (eq type 'todo))
   (org-element-map                         ; (2)
       (org-element-parse-buffer 'headline) ; (1)
       'headline
     (lambda (h)
       (org-element-property :todo-type h)))))

(defun vulpea-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-project-p)
              (setq tags (cons "project" tags))
            (setq tags (remove "project" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

(defun vulpea-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (file-truename (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun vulpea-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (like tag (quote "%\"project\"%"))]))))

(defun vulpea-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (vulpea-project-files)))

;;;###autoload
(defun vulpea-agenda-person ()
  "Show main `org-agenda' view."
  (interactive)
  (let* ((person (vulpea-select-from
                  "Person"
                  (vulpea-db-query-by-tags-some '("people"))))
         (node (org-roam-node-from-id (vulpea-note-id person)))
         (names (cons (org-roam-node-title node)
                      (org-roam-node-aliases node)))
         (tags (seq-map #'vulpea--title-to-tag names))
         (query (string-join tags "|")))
    (let ((org-agenda-overriding-arguments (list t query)))
      (org-agenda nil "M"))))

;;;###autoload
(defun vulpea-capture-task ()
  "Capture a task."
  (interactive)
  (org-capture nil "t"))

;;;###autoload
(defun vulpea-capture-meeting ()
  "Capture a meeting."
  (interactive)
  (org-capture nil "m"))

;;;###autoload
(defun vulpea-capture-note ()
  "Capture a note."
  (interactive)
  (org-capture nil "n"))

(defun vulpea-capture-meeting-template ()
  "Return a template for a meeting capture."
  (let ((person (vulpea-select
                 "Person"
                 :filter-fn
                 (lambda (note)
                   (let ((tags (vulpea-note-tags note)))
                     (seq-contains-p tags "people"))))))
    (org-capture-put :meeting-person person)
    (if (vulpea-note-id person)
        "* MEETING [%<%Y-%m-%d %a>] :REFILE:MEETING:\n%U\n\n%?"
      (concat "* MEETING with "
              (vulpea-note-title person)
              " on [%<%Y-%m-%d %a>] :MEETING:\n%U\n\n%?"))))

(defun vulpea-capture-meeting-target ()
  "Return a target for a meeting capture."
  (let ((person (org-capture-get :meeting-person)))
    ;; unfortunately, I could not find a way to reuse
    ;; `org-capture-set-target-location'
    (if (vulpea-note-id person)
        (let ((path (vulpea-note-path person))
              (headline "Meetings"))
          (set-buffer (org-capture-target-buffer path))
          ;; Org expects the target file to be in Org mode, otherwise
          ;; it throws an error. However, the default notes files
          ;; should work out of the box. In this case, we switch it to
          ;; Org mode.
          (unless (derived-mode-p 'org-mode)
            (org-display-warning
             (format
              "Capture requirement: switching buffer %S to Org mode"
              (current-buffer)))
            (org-mode))
          (org-capture-put-target-region-and-position)
          (widen)
          (goto-char (point-min))
          (if (re-search-forward
               (format org-complex-heading-regexp-format
                       (regexp-quote headline))
               nil t)
              (beginning-of-line)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "* " headline "\n")
            (beginning-of-line 0)))
      (let ((path vulpea-capture-inbox-file))
        (set-buffer (org-capture-target-buffer path))
        (org-capture-put-target-region-and-position)
        (widen)))))

;;;###autoload
(defun vulpea-agenda-skip-habits ()
  "Skip habits in agenda."
  (when (string= (org-entry-get nil "STYLE") "habit")
    (save-excursion (or (outline-next-heading) (point-max)))))

;;;###autoload
(defun vulpea-agenda-skip-non-tasks ()
  "Skip entries that are not tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((member (org-get-todo-state) org-done-keywords) next-headline)
       ((not (org-get-todo-state)) next-headline)
       (t nil)))))

;;;###autoload
(defun vulpea-agenda-skip-non-projects ()
  "Skip entries that are not projects."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (vulpea-project-p)
          nil
        next-headline))))

;;;###autoload
(defun vulpea-agenda-skip-non-stuck-projects ()
  "Skip entries that are not stuck projects."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (and (vulpea-project-p)
               (not (save-excursion
                      (org-goto-first-child)
                      (re-search-forward org-not-done-heading-regexp next-headline t))))
          nil
        next-headline))))

(add-hook 'vulpea-insert-handle-functions
          #'my-vulpea-insert-handle)

(add-hook 'find-file-hook #'vulpea-project-update-tag)
(add-hook 'before-save-hook #'vulpea-project-update-tag)

(advice-add 'org-agenda :before #'vulpea-agenda-files-update)
(advice-add 'org-todo-list :before #'vulpea-agenda-files-update)
