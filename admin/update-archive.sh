#!/bin/sh -x

makelog=""
buildir="$(pwd)"

export LANG=C
case "$1" in
    "--batch")
        makelog="$(pwd)/make.log"
        exec >"$makelog" 2>&1
        ;;
esac

# Send an email to warn about a problem.
signal_error () {
    title="$*"
    if [ "" = "$makelog" ]; then
        echo "Error: $title"
    else
        mx_gnu_org="$(host -t mx gnu.org | sed 's/.*[ 	]//')"
        (sleep 5; echo "HELO elpa.gnu.org"
         sleep 1; echo "MAIL FROM: <elpa@elpa.gnu.org>"
         sleep 1; echo "RCPT TO: <emacs-elpa-diffs@gnu.org>"
         sleep 1; echo "DATA"
         sleep 1; cat <<ENDDOC
From: ELPA update <elpa@elpa.gnu.org>
To: emacs-elpa-diffs@gnu.org
Subject: $title

ENDDOC
         cat "$makelog"
         echo "."; sleep 1) | telnet "$mx_gnu_org" smtp
    fi
    exit 1
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

rsync -av --delete --exclude=ChangeLog --exclude=.git ../elpa/packages ./

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
 tar zcf "$latest" packages)
(cd ../
 mkdir -p staging/packages
 # Not sure why we have `staging-old', but let's keep it for now.
 rm -rf staging-old
 cp -a staging staging-old
 # Move new files into place but don't throw out old package versions.
 for f in build/archive/packages/*; do
     dst="staging/packages/$(basename "$f")"
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
                 # FIXME: Announce the new package/version on
                 # gnu.emacs.sources!
                 mv "$f" "$dst"
             fi ;;
     esac
 done
 mv build/archive/"$latest" staging/
 rm -rf build/archive)

# Make the HTML and readme.txt files.
(cd ../staging/packages
 emacs --batch -l ../../build/admin/archive-contents.el \
       --eval '(batch-html-make-index)')
