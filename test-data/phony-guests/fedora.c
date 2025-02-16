/* libguestfs test images
 * Copyright (C) 2009-2025 Red Hat Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

/* This is "just enough" of a binary to look like RPM and dracut, as
 * far as virt-v2v is concerned.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* NB: This is also defined in make-fedora-img.pl */
#define KVER "5.19.0-0.rc1.14.fc37.x86_64"

static const char *
get_basename (const char *str)
{
  const char *ret = strrchr (str, '/');
  return ret == NULL ? str : ret + 1;
}

int
main (int argc, char *argv[])
{
  if (argc == 3 &&
      strcmp (get_basename (argv[0]), "rpm") == 0 &&
      strcmp (argv[1], "-ql") == 0 &&
      strncmp (argv[2], "kernel-", 7) == 0) {
    /* XXX These files and directories actually exist.  It would be
     * better to list files in /boot and /lib/modules matching a
     * pattern rather than hard-coding the list here, which duplicates
     * information in make-fedora-img.pl.
     */
    printf ("/boot/vmlinuz-" KVER "\n");
    printf ("/lib/modules/" KVER "\n");
    printf ("/lib/modules/" KVER "/kernel\n");
    printf ("/lib/modules/" KVER "/kernel/drivers\n");
    printf ("/lib/modules/" KVER "/kernel/drivers/block\n");
    printf ("/lib/modules/" KVER "/kernel/drivers/block/virtio_blk.ko\n");
  }
  else if (argc >= 1 &&
           strcmp (get_basename (argv[0]), "dracut") == 0) {
    // do nothing, pretend to rebuild the initramfs
  }
  else {
    fprintf (stderr, "phony Fedora: unknown command\n");
    exit (1);
  }

  exit (0);
}
