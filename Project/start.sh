export MSYS_NO_PATHCONV=1

docker build . -t inls625

docker run -d --name inls625 \
-e PASSWORD=password \
-v //c/Users/drdre/INLS625:/home/rstudio/project \
-v //c/Users/drdre/INLS625/.ssh:/home/rstudio/.ssh \
-v //c/Users/drdre/INLS625/.gitconfig:/home/rstudio/.gitconfig \
-p 8787:8787 \
rocker/verse
inls625