# GitHub PRs (labeling)

```sh
gh search prs --author <user> --repo <owner/repo> --updated ">=YYYY-MM-DD" --json number,title,state,createdAt,updatedAt
```

Use PR titles (and the first lines of the body if the title is vague) to write notes. A PR opened or merged on a day with no stretch near it is an orphan: list it under "How long did this take?".
