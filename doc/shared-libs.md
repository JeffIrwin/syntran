
# Shared library disclaimer

I _think_ all of the binary packages should work.  I've made an attempt to
include the required shared libraries (`.so` for Linux, `.dll` for Windows, or
`.dylib` for macOS).  However, on Windows and more so on macOS, I have only done
limited testing outside of dev machines.  That means I might have missed some
dependencies and you might have to install a particular version of gfortran or
gcc to get things working.  Please report any issues!

On Linux I'm a little more confident about dependencies, because I tested the
release in [minimal docker environments on various
distros](https://github.com/JeffIrwin/syntran/tree/main/docker).  Currently 7
distros are known to work.  There are a few that I know don't work, e.g. rocky 8
and alpine, because they don't have compatible versions of glibc

