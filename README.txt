Motion detection plugin skeleton for erlyvideo.

This is a skeleton for creation of motion detecting plugins for erlyvideo.
Skeleton itself is loosely based on plugin, developed as san example in an article of Yuri Zhloba, https://github.com/yzh44yzh/erly-presence


INSTALL

modify erlyvideo.conf:
- add erlywatcher module to modules:
  {modules, [erlywatcher]}.

Copy erlywatcher/ebin/* into /path/to/erlyvideo/../erlywatcher/ebin

After this restart erlyvideo and erlywatcher application should be started among others.