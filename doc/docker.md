
# Running syntran in docker

Build the syntran docker container and run it:
```
sudo docker build . -t sy
sudo docker run -it sy
```
This starts with syntran as the entrypoint by default.

<!-- Docker intercepts Ctrl+C, so the only way to exit syntran from within docker is
with `exit(0);`. -->

You can also edit and run syntran scripts in a container.  To start with an `sh`
shell as an entry point, run this:
```
sudo docker run -it --entrypoint /bin/sh sy
```
Then, within the container's shell, you can run any shell command, including
syntran:
```
syntran --help
```

