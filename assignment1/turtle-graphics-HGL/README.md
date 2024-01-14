Assignment 1 stub using HGL
===========================

2024-01-14: Not recommended for Windows.
- Under Windows, the HGL package only builds with GHC 8.0 and below.
  It uses the package Win32 shipped with GHC that changed its interface in the version 2.5 shipped with GHC 8.2.
- Under Windows 10, it does not produce any output, it seems.

Under macOS/Linux, it builds fine even with the latest GHC (9.8.1).
To run it, one needs an X11 installation.
