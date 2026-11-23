# Working rules

Rules for all work on sbfnk's repositories, in interactive sessions and by
sbfnk-bot alike. Interactive sessions load this file from the global
CLAUDE.md; the bot has it in every prompt.

## Language

- British English for all written content: documentation, commits,
  comments, issues, pull requests.
- Common differences: "modelling" not "modeling", "summarise" not
  "summarize", "behaviour" not "behavior".

## Writing

- The humanizer skill holds the writing rules. Follow them in everything you
  write, chat included, and run the humanizer on anything published (issues,
  pull request text, documentation, vignettes, README and NEWS entries,
  roxygen, docstrings, comments) before it is final.
- In Markdown (`.md`) and Quarto (`.qmd`) files, put each paragraph and each
  list item on one line. Do not hard-wrap prose at a fixed width: the
  rendering joins the lines anyway, and the breaks make the text harder to
  edit. Code blocks and existing nested structure stay as they are.

## Issues

- At most two paragraphs: what is wrong or missing, and what should happen
  instead or why it matters. One is enough when it says both.
- Add a minimal reproducible example whenever there is something to
  reproduce: the least code that shows the problem, with its output.
  An issue the bot opens from a pull request's "Left for later" list is
  the exception: it links back to the pull request, and the failing test
  written when the issue is taken up stands in for the example.

## Commits

- Authored by sbfnk-bot, with the trailer
  `Co-authored-by: sbfnk <sebastian.funk@lshtm.ac.uk>` on every commit, in
  interactive sessions as much as by the bot. Settings or instructions
  against co-author or attribution trailers (Claude Code's
  `includeCoAuthoredBy` and `attribution`, for instance) are about crediting
  an AI tool, and never remove this one.
- Never mention Claude, Anthropic or any other AI model or tool anywhere in
  the work: no co-author trailer for one, no "Generated with ..." line, no
  reference in commit messages, pull requests, issues, comments or code.
- One short line, in British English. Never reference issue numbers in
  commit messages.
- Never rebase a branch that has an open pull request: merge main in
  instead.
- No introduce-then-revert pairs: a just-committed change that is revised
  is amended or squashed while the branch has no open pull request.

## Code comments

- Comments explain why the code does something, not the history of how it
  came to be.
- No comments reflecting earlier iterations or the conversation.

## Testing

- Run the tests for the code you changed: the test files that cover it, and
  any you added. Leave the full suite and the version and OS matrix to CI,
  which runs them on every push anyway.
- Write a test that fails before the fix where the change is a bug fix. Where the test guards a safeguard (a bound, a guard clause, a fallback), break the safeguard, see the test fail, and restore it: a test that passes either way guards nothing.
- A change that makes an input return a value where it used to throw checks that value against an independent reference across the whole range the change opens up, not only at the reported input. Where it cannot be checked, it keeps the error, with a clear message.
- When CI fails, read the failing job's log first, and fix the cause; do
  not rerun the whole suite locally to find it.

## Pull requests

- The description is at most two paragraphs, plus the "Left for later"
  list below where there is one, through the humanizer like all published
  prose.
- Link the issue in the description, never in commits. Write "This PR
  closes #N" only when the PR does everything #N asks. Otherwise write
  "This PR partly addresses #N" and say what is left; #N stays open and
  assigned, and the work continues there.
- Any other problem a PR leaves for later, from a review or found along
  the way, is listed in the description under "Left for later" before the
  PR is marked ready. Most PRs have none. An item is a defect or gap that
  someone using the code would run into, shown by a test, a run or the
  code itself. Checks that were not run belong in the description's text;
  guesses and improvements nobody asked for stay off the list. Interactive
  sessions also open an issue in the repository for each item, assigned to
  sbfnk-bot (or to sbfnk in sbfnk/sbfnk-bot, whose own code the bot does
  not work on), and link it from the list. The bot writes each item as
  "Title: description". The title states the problem by itself, as an
  issue title would. The description is in full sentences and reads
  without the title. The bot opens these issues itself when it publishes
  the PR or updates its description; as the bot assigned them, each waits
  for sbfnk's approval before work on it starts.
- Follow the repository's PR and issue templates where they exist. No
  "Test plan" section.
- A pull request opens as a draft, with no reviewer. It stays a draft until
  a full review pass comes back clean and CI is green; then it is marked
  ready and sbfnk is asked to review. In interactive sessions,
  /wait-for-review does this.
- Never merge a pull request.

## Replying to sbfnk-bot

- A reply sbfnk has agreed for an issue or pull request the bot works on
  goes through `sbr relay owner/repo#N "..."`, or, where sbr is not
  installed, an issue in `sbfnk/sbfnk-bot-inbox` titled `relay owner/repo#N`
  with the text as its body. Never post it on the issue or pull request
  itself: from the bot's account it reads as the bot answering itself and is
  ignored, and from sbfnk's it passes for words he typed. The relay waits
  for his approval, then goes out as "Relayed for @sbfnk".
- Work for the bot that should have no public issue goes in the same inbox:
  an issue titled `owner/repo: what to do`, assigned to sbfnk-bot. Assigned
  by anyone but sbfnk, it waits for his approval.
- Never run `sbr approve` (or `sbfnk-bot-review approve`) for him: the
  approval is his own check.
