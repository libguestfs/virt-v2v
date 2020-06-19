/* virt-v2v
 * Copyright (C) 2020 Red Hat Inc.
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
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

/**
 * Mini interface to libosinfo.
 */

#include <config.h>

#include <osinfo/osinfo.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/custom.h>

#include <stdio.h>

#pragma GCC diagnostic ignored "-Wmissing-prototypes"

#define MAKE_VERSION_HEX(maj, min, mic) \
    (((maj) << 16) | ((min) <<  8) | ((mic) << 0))
#define V2V_LIBOSINFO_VERSION_HEX \
    MAKE_VERSION_HEX(OSINFO_MAJOR_VERSION, OSINFO_MINOR_VERSION, OSINFO_MICRO_VERSION)
#define IS_LIBOSINFO_VERSION(maj, min, mic) \
    (V2V_LIBOSINFO_VERSION_HEX >= MAKE_VERSION_HEX(maj, min, mic))

/*
 * libosinfo 1.8.0 provides auto-cleanup functions for all its classes,
 * so avoid declaring our own.
 */
#if !IS_LIBOSINFO_VERSION(1, 8, 0)
G_DEFINE_AUTOPTR_CLEANUP_FUNC(OsinfoFilter, g_object_unref)
G_DEFINE_AUTOPTR_CLEANUP_FUNC(OsinfoLoader, g_object_unref)
/*
 * Because of a bug in OsinfoList in libosinfo 1.7.0 (fixed in 1.8.0),
 * and a glib auto-cleanup addition for Module classes in 2.63.3,
 * avoid declaring this when:
 * - libosinfo is >= 1.7.0 and < 1.8.0
 * - glib is >= 2.63.3
 * (the 1.8.0 check is not done, as already covered by the check above)
 */
#if !IS_LIBOSINFO_VERSION(1, 7, 0) || !GLIB_CHECK_VERSION(2, 63, 3)
G_DEFINE_AUTOPTR_CLEANUP_FUNC(OsinfoList, g_object_unref)
#endif
G_DEFINE_AUTOPTR_CLEANUP_FUNC(OsinfoOsList, g_object_unref)
#endif

typedef OsinfoDb *OsinfoDb_t;
typedef OsinfoOs *OsinfoOs_t;

/* Wrap and unwrap handles, with a finalizer. */
#define OsinfoDb_t_val(rv) (*(OsinfoDb_t *)Data_custom_val(rv))

#define _OsinfoOs_t_val(rv) (*((OsinfoOs_t *)Data_custom_val(rv)))
#define OsinfoOs_t_val(rv) _OsinfoOs_t_val(Field((rv),0))

static void
OsinfoDb_t_finalize (value tv)
{
  OsinfoDb_t t = OsinfoDb_t_val (tv);
  if (t) g_object_unref (t);
}

static struct custom_operations db_custom_operations = {
  (char *) "OsinfoDb_t_custom_operations",
  OsinfoDb_t_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
};

static value
Val_OsinfoDb_t (OsinfoDb_t t)
{
  CAMLparam0 ();
  CAMLlocal1 (rv);

  rv = caml_alloc_custom (&db_custom_operations,
                          sizeof (OsinfoDb_t), 0, 1);
  OsinfoDb_t_val(rv) = t;

  CAMLreturn (rv);
}

static struct custom_operations os_custom_operations = {
  (char *) "OsinfoOs_t_custom_operations",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
};

static value
Val_OsinfoOs_t (value dbv, OsinfoOs *os)
{
  CAMLparam1 (dbv);
  CAMLlocal2 (rv, v);

  v = caml_alloc_custom (&os_custom_operations,
                         sizeof(OsinfoOs_t), 0, 1);
  _OsinfoOs_t_val (v) = os;
  rv = caml_alloc_tuple (2);
  Store_field (rv, 0, v);
  Store_field (rv, 1, dbv);

  CAMLreturn (rv);
}

value
v2v_osinfo_db_load (value unitv)
{
  CAMLparam1 (unitv);
  CAMLlocal1 (rv);
  g_autoptr(OsinfoLoader) loader = NULL;
  OsinfoDb *db = NULL;
  g_autoptr(GError) error = NULL;

  loader = osinfo_loader_new ();
  osinfo_loader_process_default_path (loader, &error);
  if (error != NULL)
    caml_failwith (error->message);

  db = osinfo_loader_get_db (loader);
  g_object_ref (db);

  rv = Val_OsinfoDb_t (db);

  CAMLreturn (rv);
}

value
v2v_osinfo_os_find_os_by_short_id (value dbv, value osv)
{
  CAMLparam2 (dbv, osv);
  CAMLlocal1 (rv);
  g_autoptr(OsinfoFilter) filter = NULL;
  g_autoptr(OsinfoOsList) os_list = NULL;
  g_autoptr(OsinfoList) list = NULL;
  OsinfoOs *os;

  os_list = osinfo_db_get_os_list (OsinfoDb_t_val (dbv));
  filter = osinfo_filter_new ();
  osinfo_filter_add_constraint (filter, OSINFO_PRODUCT_PROP_SHORT_ID, String_val (osv));
  list = osinfo_list_new_filtered (OSINFO_LIST(os_list), filter);

  if (osinfo_list_get_length (list) == 0)
    caml_raise_not_found ();

  os = OSINFO_OS(osinfo_list_get_nth (list, 0));
  rv = Val_OsinfoOs_t (dbv, os);

  CAMLreturn (rv);
}

value
v2v_osinfo_os_get_id (value osv)
{
  CAMLparam1 (osv);
  const gchar *id;

  id = osinfo_entity_get_id (OSINFO_ENTITY(OsinfoOs_t_val (osv)));
  CAMLreturn (caml_copy_string (id));
}

static value
glist_to_value_list (GList *list)
{
  CAMLparam0 ();
  CAMLlocal2 (rv, v);
  GList *l;

  rv = Val_emptylist;
  for (l = list; l != NULL; l = l->next) {
    v = caml_alloc (2, 0);
    Store_field (v, 0, caml_copy_string (l->data));
    Store_field (v, 1, rv);
    rv = v;
  }

  CAMLreturn (rv);
}

value
v2v_osinfo_os_get_device_drivers (value osv)
{
  CAMLparam1 (osv);
  CAMLlocal3 (rv, v, vi);
  OsinfoDeviceDriverList *list;
  gint i, len;

  list = osinfo_os_get_device_drivers (OsinfoOs_t_val (osv));
  len = osinfo_list_get_length (OSINFO_LIST(list));

  rv = Val_emptylist;
  for (i = len - 1; i >= 0; --i) {
    OsinfoDeviceDriver *driver;
    const gchar *str;
    gboolean b;
    GList *l;
    gint64 i64;

    driver = OSINFO_DEVICE_DRIVER(osinfo_list_get_nth (OSINFO_LIST(list), i));

    vi = caml_alloc (6, 0);
    str = osinfo_device_driver_get_architecture (driver);
    Store_field (vi, 0, caml_copy_string (str));
    str = osinfo_device_driver_get_location (driver);
    Store_field (vi, 1, caml_copy_string (str));
    b = osinfo_device_driver_get_pre_installable (driver);
    Store_field (vi, 2, Val_bool (b));
    b = osinfo_device_driver_get_signed (driver);
    Store_field (vi, 3, Val_bool (b));
#if IS_LIBOSINFO_VERSION(1, 7, 0)
    i64 = osinfo_device_driver_get_priority (driver);
#else
    /* Same as OSINFO_DEVICE_DRIVER_DEFAULT_PRIORITY in libosinfo 1.7.0+. */
    i64 = 50;
#endif
    Store_field (vi, 4, caml_copy_int64 (i64));
    l = osinfo_device_driver_get_files (driver);
    Store_field (vi, 5, glist_to_value_list (l));
    g_list_free (l);

    v = caml_alloc (2, 0);
    Store_field (v, 0, vi);
    Store_field (v, 1, rv);
    rv = v;
  }

  CAMLreturn (rv);
}
