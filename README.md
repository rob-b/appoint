# appoint

(Eventually) view open PRs and assign them.

Note that this was written for internal usage with private repos and so, for
now at least, requires that you create a github auth token and use that to set
`GITHUB_TOKEN`. appoint will use that token to make all requests via OAuth.

Installation
============

Clone this repo then run `stack build --copy-bins`

Example Usage
=============

```sh
appoint thoughtbot paperclip
```
