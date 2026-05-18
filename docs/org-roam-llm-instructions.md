# Org-roam LLM instructions

Rules for reading and writing `~/org-roam/*.org` files. Apply to any
session that touches org-roam, regardless of which project's CLAUDE.md
brought you here.

## Core principle

Top-level headings belong to the human. AI content lives one level
down, tagged with `:ai:`. The human controls the structure; AI fills
in detail underneath.

## Rules for writing

1. **Never modify untagged content.** Human-written sections are
   read-only unless explicitly asked to edit them.

2. **Always tag AI content with `:ai:`.** Every heading or TODO that
   AI creates or maintains must have the `:ai:` tag at the end of the
   heading line.

3. **Be terse.** Bullet points, not paragraphs. One line per point.
   The human will read this — respect their time.

4. **Preserve existing structure.** Don't reorganise headings, reorder
   sections, or change filetags. Add underneath, don't restructure.

5. **Don't create new top-level headings without being asked.**

6. **British English** throughout.

## Org syntax essentials

- **Headings**: `* Level 1`, `** Level 2`, etc.
- **Tags**: `:tagname:` at end of heading line (colon-delimited).
  Multiple tags: `:tag1:tag2:`.
- **TODOs**: `** TODO task description` — states are TODO, DONE, KILL.
  Use org TODO/DONE keywords, not `- [ ]` / `- [X]` checkboxes, for
  any tracked action items.
- **Timestamps**: `<2026-03-18 Wed>` — always include day-of-week.
- **Description lists**: `- Term :: Description`.
- **Links**: `[[id:UUID][Display text]]` for org-roam links.
- **Properties**: `:PROPERTIES:` drawer under heading, key-value pairs.
- **Italic**: `/italic/`. **Bold**: `*bold*` (single asterisks, not
  double).
- **Folded sections**: add `:VISIBILITY: folded` in properties drawer.

## People-node specific rules

These apply to org-roam files indexed at
`~/code/people/CLAUDE.md` (per-person supervision/development notes).

1. **Meetings are chronological, newest first** (after any
   non-meeting content). When adding a meeting entry, put it at the
   top of the `* Meetings` section.

2. **Transcripts and summaries** go as sub-headings under the meeting
   entry they belong to, tagged `:ai:transcript:` and `:ai:`
   respectively.

3. **TODOs**: AI-suggested TODOs get the `:ai:` tag. Never remove or
   mark done a human-created TODO — only the human does that.
   - Only the user's own actions are written as `*** TODO ...` org
     headings (so they feed their agenda).
   - Other people's actions (e.g. things the supervisee committed to
     do) go under a `*** <Name>'s actions` heading as plain bullet
     points — not TODOs. They are not the user's tasks and shouldn't
     show up in their agenda.

4. **Comms section**: When prepping a 1-1, populate `** Recent :ai:`
   with one-line summaries of notable emails/slack. Overwrite previous
   content (snapshot, not log).

5. **Development > Coaching notes**: Update when you learn something
   new about the person's development. Don't repeat what's already
   there.

## People-node template

```org
:PROPERTIES:
:ID:       <generated-by-org>
:END:
#+title: Firstname Lastname
#+filetags: :@FirstnameLastname:people:

* Development
/Your terse notes: role, strengths, gaps, direction./

** Coaching notes                                            :ai:
/AI-maintained: what they need from you, what to challenge,
development trajectory./

* Tasks
/Mix of your TODOs and AI-suggested ones (tagged)./

* Meetings
** MEETING with Firstname Lastname
<YYYY-MM-DD Day>
/Your notes — terse, yours, untagged./

*** Transcript                                               :ai:transcript:
:PROPERTIES:
:VISIBILITY: folded
:SOURCE:
:END:
/Full or partial transcript. Delete after a quarter./

*** Summary                                                  :ai:
:PROPERTIES:
:VISIBILITY: folded
:END:
/Dense bullet-point summary generated from transcript./

* Comms
/Your notes on important exchanges./

** Recent                                                    :ai:
/AI-populated: notable recent emails/slack, one line each./

* Projects
/Your notes on shared projects./
```

## Reading emails (~/Maildir/)

- Maildir format: `~/Maildir/{account}/{folder}/{cur,new}/` contains
  individual email files with full headers + body.
- To find emails from/to a person: grep for their email address or
  name in the relevant Maildir folders.
- When summarising for the Comms section: date, channel
  (email/slack), one-line summary. No quoting email bodies into org
  files.

## 1-1 prep workflow

When asked to prep a 1-1 with someone:

1. Read their org-roam file.
2. Check recent emails/slack for context.
3. Update `** Recent :ai:` in Comms.
4. Review Development > Coaching notes — update if needed.
5. Check their open TODOs.
6. Present a brief prep summary (don't write it into the file).

## Style

- British English throughout.
- Be terse: bullet points, not paragraphs.
- Observations are chronological, newest first.
- Never fabricate observations — only record what the user tells you.
