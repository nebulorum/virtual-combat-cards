#!/bin/sh

if [ $# -ne 1 ]; then 
	echo "Usage:\n\t$0 <version-string>" 
	exit 1
fi

VERSION=$1
INSTALLZIP="target/vcc-$VERSION-release-full.zip" 
if [ ! -f $INSTALLZIP  ]; then
    echo "Must run mvn package and generate $INSTALLZIP before running this command"
    exit 1;
fi

PLISTFILE="../vcc-main/target/classes/external/Info.plist"
if [ ! -f "$PLISTFILE" ]; then
	echo "Cant find updated Info.plist at $PLISTFILE" 
	exit 1;
fi

# Targes
BUILDIN="target/MacImage-$VERSION"
APPBUNDLE="$BUILDIN/Virtual Combat Cards.app"
JAVATGT=$APPBUNDLE/Contents/Resources/Java
IMAGE="target/MacBundle-$VERSION.dmg"

# Remove prior build
rm -f $IMAGE
rm -fr "$BUILDIN"
if [ -d $BUILDIN ]; then
	echo "Failed to clean $BUILDIN"
	exit 1
fi

# Copy bundle and unzip
mkdir -p "$JAVATGT"
unzip -d "$JAVATGT" $INSTALLZIP
(cd "$JAVATGT" && unzip install.zip && rm install.zip)
cp "$PLISTFILE" "$APPBUNDLE/Contents"
cp src/main/macos/files/PkgInfo "$APPBUNDLE/Contents"
cp src/main/macos/Resources/* "$APPBUNDLE/Contents/Resources" 

# Copy stub and resources
mkdir "$APPBUNDLE/Contents/MacOS"
cp /System/Library/Frameworks/JavaVM.framework/Versions/Current/Resources/MacOS/JavaApplicationStub "$APPBUNDLE/Contents/MacOS/JavaApplicationStub"
chmod 755 "$APPBUNDLE/Contents/MacOS/JavaApplicationStub"

#Make image
src/main/macos/create-dmg --window-size 500 300 --icon-size 96  --volname "VCC Installer" --app-drop-link 380 205 --icon "Virtual Combat Cards" 110 205 --background src/main/macos/Resources/install-bg.tiff $IMAGE $BUILDIN 
#--volicon src/main/macos/Resources/d20metal.icns
