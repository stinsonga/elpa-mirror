#/bin/bash
## package-update.sh -- Create a package archive from the elpa repository

## Usage: ./package-update.sh DEST [FULL-UPDATE]
##
## This creates a package archive beginning in DEST.
##
## The package archive itself is created in DEST/packages.  This dir
## contains the package files and the `archive-contents' listing file.
##
## If a second argument FULL-UPDATE is specified (whatever its value),
## also create the following:
##   - the archive admin scripts in DEST/admin
##   - a tarball contianing the entire archive in
##     DEST/packages/emacs-packages-latest.tgz
##   - the Org mode daily package

PATH=/bin:/usr/bin:/usr/local/bin
DEST=${1%/}
FULL=$2

EMACS=emacs
BZR=bzr

LOG=$DEST/update-log
PKGROOT=$DEST/packages
TMP_PKGROOT=$DEST/packages-new
REPO_PACKAGES=packages

## Parse arguments
if [ -z $DEST ]; then
    echo "Syntax: $0 HOMEDIR [fetch-extras-boolean]"
    exit 1
elif [ -d $DEST ]; then
    echo "Installing into '$DEST', log is '$LOG'"
    echo "Installing into '$DEST'" > $LOG
    if [ -z $FULL ]; then
	echo "Base archive update only (pass second arg for full update)."
    else
	echo "Performing full archive update."
	TARBALL=$PKGROOT/emacs-packages-latest.tgz
	TARBALL_ROOT="emacs-24.1-packages-`/bin/date +'%F'`"
	ADMINROOT=$DEST/admin
	REPO_ADMIN=admin
    fi
else
    echo "Sorry but $DEST is not a directory, aborting."
    exit 1
fi

## Change to the bzr root directory
cd $(dirname $0)
REPO_ROOT_DIR=`$BZR root`;
if [ -z $REPO_ROOT_DIR ]; then
    "This script should be run from a bzr repository, aborting."
    exit 1
else
    cd $REPO_ROOT_DIR;
fi

## Create the working directory that will be the world-facing copy of
## the package archive base.
echo "Exporting packages to temporary working directory $TMP_PKGROOT" >> $LOG
rm -rf $TMP_PKGROOT
$BZR export $TMP_PKGROOT $REPO_PACKAGES

## Call the `make-package-archive' Lisp function.

## If second arg is provided, copy in the admin directory and run the
## Org daily synch scripts
if [ -z $FULL ]; then
    echo "Skipping admin directory" >> $LOG
    echo "Skipping post-export fetchers" >> $LOG
else
    echo "Exporting admin scripts to $ADMINROOT" >> $LOG
    rm -rf $ADMINROOT
    $BZR export $ADMINROOT $REPO_ADMIN
    echo "Running post-export org-mode fetcher as '$ADMINROOT/org-synch.sh $TMP_PKGROOT $ADMINROOT'" >> $LOG
    $ADMINROOT/org-synch.sh $TMP_PKGROOT $ADMINROOT >> $LOG 2>&1
fi

## Tar up the multi-file packages.
echo "Creating multi-file package tarballs in $TMP_PKGROOT" >> $LOG
cd $TMP_PKGROOT
for pt in *; do
    if [ -d $pt ]; then
	if [ -f $pt/README ]; then
	    cp $pt/README $pt-readme.txt;
	fi
	echo "Creating tarball $TMP_PKGROOT/$pt.tar" >> $LOG
	tar -cf $pt.tar $pt --remove-files
    fi
done

## Move the working directory to its final location
echo "Moving $TMP_PKGROOT to $PKGROOT" >> $LOG
rm -rf $PKGROOT-old
if [ -d $PKGROOT ]; then
    mv $PKGROOT $PKGROOT-old
fi
mv $TMP_PKGROOT $PKGROOT
rm -rf $PKGROOT-old

## If doing a full update, make a tarball of the entire archive.
if [ -z $FULL ]; then
    echo "Skipping archive tarball" >> $LOG
else
    echo "Exporting packages into $TARBALL (root = $TARBALL_ROOT)" >> $LOG
    cd $REPO_ROOT_DIR
    $BZR export --format=tgz --root=$TARBALL_ROOT $TARBALL $REPO_PACKAGES
fi

chmod -R a+rX $PKGROOT
echo "Update complete at" `/bin/date` >> $LOG
