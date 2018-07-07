{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ ... }: {
  android.applicationId = "ca.srid.revue.dev";
  android.displayName = "Revue (srid)";
  ios.bundleIdentifier = "ca.srid.revue.dev";
  ios.bundleName = "Revue (srid";
})
