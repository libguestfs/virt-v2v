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
G_DEFINE_AUTOPTR_CLEANUP_FUNC(OsinfoOsList, g_object_unref)
G_DEFINE_AUTOPTR_CLEANUP_FUNC(OsinfoDeviceList, g_object_unref)
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
  CAMLlocal2 (rv, errv);
  g_autoptr(OsinfoLoader) loader = NULL;
  OsinfoDb *db = NULL;
  g_autoptr(GError) error = NULL;

  loader = osinfo_loader_new ();
  osinfo_loader_process_default_path (loader, &error);
  if (error != NULL) {
    char *err;

    if (asprintf (&err, "libosinfo error: "
                  "osinfo_loader_process_default_path: %s",
                  error->message) >= 0) {
      errv = caml_copy_string (err);
      free (err);
    }
    else
      errv = caml_copy_string ("osinfo_loader_process_default_path failed");

    caml_failwith_value (errv);
  }

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
  OsinfoList *list;
  OsinfoOs *os;

  os_list = osinfo_db_get_os_list (OsinfoDb_t_val (dbv));
  filter = osinfo_filter_new ();
  osinfo_filter_add_constraint (filter, OSINFO_PRODUCT_PROP_SHORT_ID, String_val (osv));
  list = osinfo_list_new_filtered (OSINFO_LIST(os_list), filter);

  if (osinfo_list_get_length (list) == 0) {
    g_object_unref (list);
    caml_raise_not_found ();
  }

  os = OSINFO_OS(osinfo_list_get_nth (list, 0));
  rv = Val_OsinfoOs_t (dbv, os);
  g_object_unref (list);

  CAMLreturn (rv);
}

value
v2v_osinfo_os_get_id (value osv)
{
  CAMLparam1 (osv);
  CAMLlocal1 (copyv);
  const gchar *id;

  id = osinfo_entity_get_id (OSINFO_ENTITY(OsinfoOs_t_val (osv)));
  copyv = caml_copy_string (id);
  CAMLreturn (copyv);
}

static value
glist_to_value_list (GList *list)
{
  CAMLparam0 ();
  CAMLlocal3 (rv, v, copyv);
  GList *l;

  rv = Val_emptylist;
  for (l = list; l != NULL; l = l->next) {
    v = caml_alloc (2, 0);
    copyv = caml_copy_string (l->data);
    Store_field (v, 0, copyv);
    Store_field (v, 1, rv);
    rv = v;
  }

  CAMLreturn (rv);
}

/* Collect OsinfoDevice properties from two levels:
 *
 * - The OSINFO_ENTITY_PROP_ID property, originating from the OsinfoEntity base
 *   class. This is a unique URI, identifying the device.
 *
 * - All currently known OSINFO_DEVICE_PROP_* properties, originating from the
 *   OsinfoDevice class.
 *
 * All of the above properties have string values. Thus, for uniformity, access
 * all these properties by their names at the OsinfoEntity level (i.e., forego
 * the class- and property-specific, dedicated property getter functions).
 */
static const char * const device_prop[] = {
  OSINFO_ENTITY_PROP_ID,
  OSINFO_DEVICE_PROP_VENDOR,
  OSINFO_DEVICE_PROP_VENDOR_ID,
  OSINFO_DEVICE_PROP_PRODUCT,
  OSINFO_DEVICE_PROP_PRODUCT_ID,
  OSINFO_DEVICE_PROP_NAME,
  OSINFO_DEVICE_PROP_CLASS,
  OSINFO_DEVICE_PROP_BUS_TYPE,
  OSINFO_DEVICE_PROP_SUBSYSTEM,
};
#define NUM_DEVICE_PROPS (sizeof device_prop / sizeof device_prop[0])

static value
v2v_osinfo_device_list_to_value_list (OsinfoDeviceList *dev_list)
{
  CAMLparam0 ();
  CAMLlocal4 (retvalv, linkv, propsv, copyv);
  OsinfoList *ent_list;
  gint ent_nr;

  retvalv = Val_emptylist;
  ent_list = OSINFO_LIST (dev_list);
  ent_nr = osinfo_list_get_length (ent_list);

  while (ent_nr > 0) {
    OsinfoEntity *ent;
    size_t prop_nr;

    --ent_nr;
    ent = osinfo_list_get_nth (ent_list, ent_nr);

    propsv = caml_alloc (NUM_DEVICE_PROPS, 0);
    for (prop_nr = 0; prop_nr < NUM_DEVICE_PROPS; ++prop_nr) {
      const gchar *prop_val;

      prop_val = osinfo_entity_get_param_value (ent, device_prop[prop_nr]);
      if (prop_val == NULL)
        prop_val = "";
      copyv = caml_copy_string (prop_val);
      Store_field (propsv, prop_nr, copyv);
    }

    linkv = caml_alloc (2, 0);
    Store_field (linkv, 0, propsv);
    Store_field (linkv, 1, retvalv);
    retvalv = linkv;
  }

  CAMLreturn (retvalv);
}

value
v2v_osinfo_os_get_device_drivers (value osv)
{
  CAMLparam1 (osv);
  CAMLlocal4 (rv, v, vi, copyv);
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
    OsinfoDeviceList *dev_list;

    driver = OSINFO_DEVICE_DRIVER(osinfo_list_get_nth (OSINFO_LIST(list), i));

    vi = caml_alloc (6, 0);
    str = osinfo_device_driver_get_architecture (driver);
    copyv = caml_copy_string (str);
    Store_field (vi, 0, copyv);
    str = osinfo_device_driver_get_location (driver);
    copyv = caml_copy_string (str);
    Store_field (vi, 1, copyv);
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
    copyv = caml_copy_int64 (i64);
    Store_field (vi, 4, copyv);
    l = osinfo_device_driver_get_files (driver);
    Store_field (vi, 5, glist_to_value_list (l));
    g_list_free (l);
    dev_list = osinfo_device_driver_get_devices (driver);
    v = (dev_list == NULL) ?
        Val_emptylist :
        v2v_osinfo_device_list_to_value_list (dev_list);
    Store_field (vi, 6, v);

    v = caml_alloc (2, 0);
    Store_field (v, 0, vi);
    Store_field (v, 1, rv);
    rv = v;
  }

  CAMLreturn (rv);
}

value
v2v_osinfo_os_get_all_devices (value osv)
{
  CAMLparam1 (osv);
  CAMLlocal1 (retvalv);
  g_autoptr (OsinfoDeviceList) dev_list = NULL;

  dev_list = osinfo_os_get_all_devices (OsinfoOs_t_val (osv), NULL);
  retvalv = v2v_osinfo_device_list_to_value_list (dev_list);
  CAMLreturn (retvalv);
}
