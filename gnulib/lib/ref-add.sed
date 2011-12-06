/^# Packages using this file: / {
  s/# Packages using this file://
  ta
  :a
  s/ pippo / pippo /
  tb
  s/ $/ pippo /
  :b
  s/^/# Packages using this file:/
}
