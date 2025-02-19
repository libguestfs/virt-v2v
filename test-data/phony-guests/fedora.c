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

/* This is "just enough" of a binary to look like /bin/sh, RPM and
 * dracut, as far as virt-v2v is concerned.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* NB: This is also defined in make-fedora-img.pl */
#define KVER "5.19.0-0.rc1.14.fc37.x86_64"

static const char *
get_basename (const char *str)
{
  const char *ret = strrchr (str, '/');
  return ret == NULL ? str : ret + 1;
}

static void
add_str (char ***argv, size_t *argc, char *str)
{
  (*argc)++;
  (*argv) = realloc (*argv, *argc * sizeof (char *));
  (*argv)[*argc-1] = str;
}

static void
add_null (char ***argv, size_t *argc)
{
  add_str (argv, argc, NULL);
}

static void
add (char ***argv, size_t *argc, const char *s, size_t len)
{
  char *copy = strndup (s, len);
  add_str (argv, argc, copy);
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
  else if (argc == 3 &&
           strcmp (get_basename (argv[0]), "sh") == 0 &&
           strcmp (argv[1], "-c") == 0) {
    /* Split the command and execute it.  Only handles trivial cases. */
    char *cmd = argv[2];
    char **cmdv = NULL;
    size_t i, cmdvlen = 0, n;
    const size_t len = strlen (cmd);

    for (i = 0; i < len;) {
      switch (cmd[i]) {
      case ' ': case '\t':
        i++;
        continue;

      case '"':
        n = strcspn (&cmd[i+1], "\"");
        add (&cmdv, &cmdvlen, &cmd[i+1], n);
        i += n+2;
        break;

      case '\'':
        n = strcspn (&cmd[i+1], "'");
        add (&cmdv, &cmdvlen, &cmd[i+1], n);
        i += n+2;
        break;

      default:
        n = strcspn (&cmd[i], " \t");
        add (&cmdv, &cmdvlen, &cmd[i], n);
        i += n;
      }
    }
    add_null (&cmdv, &cmdvlen);

    execvp (cmdv[0], cmdv);
    perror (cmdv[0]);
    exit (EXIT_FAILURE);
  }
  else {
    int i;

    fprintf (stderr, "ERROR: test-data/phony-guests/fedora.c: "
             "unexpected command:\n");
    for (i = 0; i < argc; ++i)
      fprintf (stderr, "argv[%d] = %s\n", i, argv[i]);
    exit (EXIT_FAILURE);
  }

  exit (EXIT_SUCCESS);
}
