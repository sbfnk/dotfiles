;;; tools/vulpea/config.el -*- lexical-binding: t; -*-

(use-package! vulpea
   :after org-capture
   :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable))
   :init
        (require 'vulpea-buffer)
        (require 'vulpea-db)
        (require 'vulpea-note)
        (require 'vulpea-select)
        (add-to-list 'org-tags-exclude-from-inheritance "project")
        (defvar vulpea-capture-inbox-file
          (format "inbox-%s.org" (system-name))
          "The path to the inbox file.

           It is relative to `org-directory', unless it is absolute.")
         (dolist (var '(vulpea-capture-inbox-file))
          (set var (expand-file-name (symbol-value var) org-roam-directory)))
        (unless org-default-notes-file
          (setq org-default-notes-file vulpea-capture-inbox-file))
        (setq
         org-capture-templates
         '(("t" "todo" plain (file vulpea-capture-inbox-file)
            "* TODO %?\n%U\n" :clock-in t :clock-resume t)
           ("m" "Meeting" entry
            (function vulpea-capture-meeting-target)
            (function vulpea-capture-meeting-template)
            :clock-in t
            :clock-resume t))))

(defvar vulpea-agenda-main-buffer-name "*agenda:main*"
   "Name of the main agenda buffer.")

(defvar vulpea-agenda-hide-scheduled-and-waiting-next-tasks t
  "Non-nil means to hide scheduled and waiting tasks.
Affects the following commands:
- `vulpea-agenda-cmd-focus'
- `vulpea-agenda-cmd-waiting'")

(defconst vulpea-agenda-cmd-refile
  '(tags
    "REFILE"
    ((org-agenda-overriding-header "To refile")
     (org-tags-match-list-sublevels nil))))

(defconst vulpea-agenda-cmd-today
  '(agenda
    ""
    ((org-agenda-start-day nil)
     (org-agenda-span 'day)
     (org-agenda-skip-deadline-prewarning-if-scheduled t)
     (org-agenda-sorting-strategy '(habit-down
                                    time-up
                                    category-keep
                                    todo-state-down
                                    priority-down)))))

(defconst vulpea-agenda-cmd-focus
  '(tags-todo
    "FOCUS"
    ((org-agenda-overriding-header
      (concat "To focus on"
              (if vulpea-agenda-hide-scheduled-and-waiting-next-tasks
                  ""
                " (including WAITING and SCHEDULED tasks)")))
     (org-agenda-skip-function 'vulpea-agenda-skip-habits)
     (org-tags-match-list-sublevels t)
     (org-agenda-todo-ignore-scheduled
      vulpea-agenda-hide-scheduled-and-waiting-next-tasks)
     (org-agenda-todo-ignore-deadlines
      vulpea-agenda-hide-scheduled-and-waiting-next-tasks)
     (org-agenda-todo-ignore-with-date
      vulpea-agenda-hide-scheduled-and-waiting-next-tasks)
     (org-agenda-tags-todo-honor-ignore-options t)
     (org-agenda-sorting-strategy
      '(todo-state-down priority-down effort-up category-keep)))))

(defconst vulpea-agenda-cmd-stuck-projects
  '(tags-todo
    "PROJECT-CANCELLED-HOLD/!"
    ((org-agenda-overriding-header "Stuck Projects")
     (org-agenda-skip-function 'vulpea-agenda-skip-non-stuck-projects)
     (org-agenda-sorting-strategy
      '(todo-state-down priority-down effort-up category-keep)))))

(defconst vulpea-agenda-cmd-projects
  '(tags-todo
    "PROJECT-HOLD"
    ((org-agenda-overriding-header (concat "Projects"))
     (org-tags-match-list-sublevels t)
     (org-agenda-skip-function 'vulpea-agenda-skip-non-projects)
     (org-agenda-tags-todo-honor-ignore-options t)
     (org-agenda-sorting-strategy
      '(todo-state-down priority-down effort-up category-keep)))))

(defconst vulpea-agenda-cmd-waiting
  '(tags-todo
    "-CANCELLED+WAITING-READING-FOCUS|+HOLD/!"
    ((org-agenda-overriding-header
      (concat "Waiting and Postponed Tasks"
              (if vulpea-agenda-hide-scheduled-and-waiting-next-tasks
                  ""
                " (including WAITING and SCHEDULED tasks)")))
     (org-agenda-skip-function 'vulpea-agenda-skip-non-tasks)
     (org-tags-match-list-sublevels nil)
     (org-agenda-todo-ignore-scheduled
      vulpea-agenda-hide-scheduled-and-waiting-next-tasks)
     (org-agenda-todo-ignore-deadlines
      vulpea-agenda-hide-scheduled-and-waiting-next-tasks))))
