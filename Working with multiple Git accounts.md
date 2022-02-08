# Working with multiple Git accounts
Summary:

Since I now need to use two git accounts, and don't want to have to sign in and out all the time, I used multiple SSH keys tied to different git accounts
located on the same computer (this will require a separate setup for the laptop btw!).

After setting that up, I encountered a major problem. Despite the authenticator most certainly recognizing the right SSH key for the repository owners account, the displayed author
on every commit was the account that did not have access to the repository (also that said other account shows no history of contributing to this repos, even with the repo being public.

I was able to solve this by altering the .config file for repos to have the correct username and email for the owners profile, and suddenly the correct profile was shown as the author.

As a consequence of this, GitHub desktop (which I use most of the time) is now completely out of its water, so I'll just use git bash to push commits.


---
https://newbedev.com/running-ssh-agent-when-starting-git-bash-on-windows
https://www.atlassian.com/git/tutorials/git-ssh
https://stackoverflow.com/questions/64043238/enter-pin-for-authenticator-issue-related-to-ssh
https://stackoverflow.com/questions/69508635/why-doesnt-changing-my-ssh-key-make-the-correct-author-be-shown-for-my-commits


https://www.freecodecamp.org/news/manage-multiple-github-accounts-the-ssh-way-2dadc30ccaca/
