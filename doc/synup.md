# synup, the toolchain manager

`synup` installs several versions of syntran side by side, and picks which one
runs when you type `syntran`.  It's a separate small program, like `rustup` is
to `rustc`, so that a syntran that you switched to or from can never get in the
way of switching back.  Every release zip has it next to `syntran`.

It needs `curl` for downloads, and `unzip` (or a `tar` that reads zips, like
macOS's and Windows's) to unpack them.  It doesn't link libcurl.

## Getting started

Unzip a release, and run `synup init` from there:

```
./synup init
```

That makes `~/.syntran`, copies `synup` into `~/.syntran/bin`, installs the
latest release, and makes it the default.  Then put `~/.syntran/bin` in your
`PATH`.  `init` doesn't edit your shell's startup files, but it prints the line
to add.  If there's already a `syntran` in your `PATH` somewhere else, e.g. one
in `~/.local/bin`, it's found first until `~/.syntran/bin`
comes before it.

Set `SYNUP_HOME` to use a different dir.  It's `%USERPROFILE%\.syntran` on
Windows.

## Commands

| Command | |
| --- | --- |
| `synup init` | Set up `$SYNUP_HOME` with the latest release |
| `synup install [VERSION]` | Install `latest` (the default), `dev`, or a version like `1.7.0` |
| `synup default <VERSION>` | Run this version as `syntran`: `stable`, `dev`, or a version |
| `synup update` | Install the newest release if there's a new one, and refresh `dev` if it's installed |
| `synup list [--available]` | Show what's installed, or the releases that could be |
| `synup remove <VERSION>` | Delete an installed version, unless it's the default |
| `synup run <VERSION> [ARGS]...` | Run an installed version once, without changing the default |

`stable` means the newest installed release, so after `synup update`, a default
of `stable` moves to the new release by itself.  Installing an older release
doesn't change the default.

`syntran --update` runs `synup update`, which needs `synup` in your `PATH`.

## Where things go

```
~/.syntran/
    bin/synup
    bin/syntran            a link to the default version (a copy on Windows)
    versions/1.6.0/        the contents of the release zip
    versions/1.7.0/
    versions/dev/
    default                `stable`, `dev`, or a version
```

`bin/syntran` is replaced atomically, so it's safe to run `synup default` or
`synup update` while a syntran is running.  On Windows, which can't replace a
running exe, the running one is renamed to `syntran.exe.old` and deleted by the
next `synup` command.

## Checksums

Every release has a `SHA256SUMS` file next to its zips, in the format of
`sha256sum`.  `synup` downloads it with the zip and refuses to unpack anything
if it's missing, has no line for the zip, or doesn't match.  It's the same for
`dev`.  The checks protect against a corrupted or half-uploaded download.  They
don't protect against someone who can change both files on GitHub, since both
come from the same place.

## The dev build

Every push to the `dev` branch that passes CI replaces the assets of the
`dev-latest` prerelease.  `synup install dev` downloads that, and `synup update`
refreshes it if it's installed.  It's a prerelease, so it never becomes the
`latest` release.

## For maintainers

`synup` depends on these not changing: the asset names `syntran-linux.zip`,
`syntran-macos.zip`, `syntran-windows.zip`, and `SHA256SUMS`; the URLs
`https://github.com/JeffIrwin/syntran/releases/latest/download/<asset>` and
`.../releases/download/<tag>/<asset>`; tags that are bare versions (`1.7.0`);
and the `dev-latest` tag.

When you publish a release by hand, upload the zips, and then make and upload
the checksums:

```
bash utils/release-sums.sh syntran-linux.zip syntran-macos.zip syntran-windows.zip
gh release upload <tag> SHA256SUMS
```

Without `SHA256SUMS`, `synup install <tag>` refuses.  The same goes for old
releases, so one that was published before `synup` can be given a `SHA256SUMS`
afterward.

Tests:  `unit_test_synup` in `src/tests/test.f90` covers the checksum, version,
and parsing code.  `utils/test-synup.sh path/to/synup` runs every command
offline, against a fake release tree that curl serves with `file://`, through
`SYNUP_RELEASES_URL`.  It doesn't run on Windows yet, since its stub syntran is
a shell script.
