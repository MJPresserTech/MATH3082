CSE 3007 - Python for Data Science repo

Helpful code:

$ git add . # adds shit to commit list
$ git commit -m "comment" # commits shit
$ git push -u origin main # pushes shit
$ git pull origin main # pulls shit

add --force to force shit

$ git fetch --all
$ git reset --hard origin/main # force pull basically

If it breaks? Corrupted trees n shit
$ find .git/objects/ -size 0 -exec rm -f {} \;
$ git fetch origin

note to self:
sometimes VS Code just disapears? Just reinstall it ig