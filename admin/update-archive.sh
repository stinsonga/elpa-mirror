#!/bin/sh -x

makelog=""
buildir="$(pwd)"

announce=no
a_email="" #info-gnu-emacs@gnu.org

export LANG=C
while [ $# -gt 0 ]; do
    case "$1" in
        "--announce") announce=yes; a_email="$2"; shift ;;
        "--batch")
            makelog="$(pwd)/make.log"
            exec >"$makelog" 2>&1
            ;;
    esac
    shift
done

send_mail () {
    to="$1"; shift
    title="$*"
    (cat <<ENDDOC
From: ELPA update <do.not.reply@elpa.gnu.org>
To: $to
Subject: $title

ENDDOC
     cat -) | /usr/sbin/sendmail "$to"
}

# Send an email to warn about a problem.
signal_error () {
    title="$*"
    if [ "" = "$makelog" ]; then
        echo "Error: $title"
    else
        send_mail "emacs-elpa-diffs@gnu.org" "$title" <"$makelog"
    fi
    exit 1
}

announce_new () {
    if [ "yes" != "$announce" ]; then return; fi
    pv="$1"
    pkg="$(echo "$pv" | sed -e 's/^\(.*\)-\([^-]*\)\.[^-.]*$/\1/')"
    ver="$(echo "$pv" | sed -e 's/^\(.*\)-\([^-]*\)\.[^-.]*$/\2/')"
    if [ -z "$pkg" ] || [ -z "$ver" ]; then signal_error "bad PKG-VER: $pv"; fi
    send_mail "$a_email" "[GNU ELPA] $pkg version $ver" <<ENDDOC
Version $ver of GNU ELPA package $pkg has just been released.
You can now find it in M-x package-list RET.

More at http://elpa.gnu.org/packages/$pkg.html
ENDDOC
}

cd ../elpa

# Fetch changes.
git pull || signal_error "git pull failed"

# Remember we're inside the "elpa" branch which we don't want to trust,
# So always refer to the makefile and admins files from $builddir".

# Setup and update externals.
emacs --batch -l "$buildir/admin/archive-contents.el" \
      -f archive-add/remove/update-externals

make -f "$buildir/GNUmakefile" check_copyrights ||
    signal_error "check_copyright failed"

cd "$buildir"

rsync -av --delete \
      --exclude=ChangeLog \
      --exclude=.git \
      --exclude='*.elc' \
      --exclude='*~' \
      --exclude='*-autoloads.el' \
      ../elpa/packages ./

# Refresh the ChangeLog files.  This needs to be done in
# the source tree, because it needs the VCS data!
emacs -batch -l admin/archive-contents.el \
      -eval '(archive-prepare-packages "../elpa")'


rm -rf archive                  # In case there's one left over!
make archive-full || {
    signal_error "make archive-full failed"
}
latest="emacs-packages-latest.tgz"
(cd archive
 GZIP=--best tar zcf "$latest" packages)
(cd ../
 mkdir -p staging/packages
 # Not sure why we have `staging-old', but let's keep it for now.
 rsync -av --inplace --delete staging/. staging-old/.
 # Move new files into place but don't throw out old package versions.
 for f in build/archive/packages/*; do
     # PKG-VER
     pv=$(basename "$f")
     dst="staging/packages/$pv"
     # Actually, let's never overwrite an existing version.  So changes can
     # be installed without causing a new package to be built until the
     # version field is changed.  Some files need to be excluded from the
     # "immutable" policy, most importantly "archive-contents"
     # and "*-readme.txt".
     case $dst in
         */archive-contents | *-readme.txt ) mv "$f" "$dst" ;;
         * ) if [ -r "$dst" ]
             then rm "$f"
             else
                 mv "$f" "$dst"
                 # FIXME: Add a tag to remember the precise code used.
                 announce_new "$pv"
             fi ;;
     esac
 done
 mv build/archive/"$latest" staging/
 rm -rf build/archive)

# Make the HTML and readme.txt files.
(cd ../staging/packages
 emacs --batch -l ../../build/admin/archive-contents.el \
       --eval '(batch-html-make-index)')
