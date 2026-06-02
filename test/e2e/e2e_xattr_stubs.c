#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#if defined(__linux__)
#include <sys/types.h>
#include <sys/xattr.h>
#endif

static int e2e_xattr_missing(int error_code) {
#ifdef ENODATA
  if (error_code == ENODATA) {
    return 1;
  }
#endif
#ifdef ENOATTR
  if (error_code == ENOATTR) {
    return 1;
  }
#endif
#ifdef ENOKEY
  if (error_code == ENOKEY) {
    return 1;
  }
#endif
#if defined(__linux__)
  if (error_code == 127) {
    return 1;
  }
#endif
  return 0;
}

CAMLprim value caml_e2e_setxattr(value path_v, value name_v, value value_v) {
  CAMLparam3(path_v, name_v, value_v);
#if defined(__linux__)
  if (setxattr(String_val(path_v), String_val(name_v), String_val(value_v),
               caml_string_length(value_v), 0) == -1) {
    caml_uerror("setxattr", path_v);
  }
  CAMLreturn(Val_unit);
#else
  caml_failwith("xattrs are not supported by this environment");
#endif
}

CAMLprim value caml_e2e_getxattr(value path_v, value name_v) {
  CAMLparam2(path_v, name_v);
  CAMLlocal1(result);
#if defined(__linux__)
  size_t size = 1024;
  while (1) {
    char *buffer = malloc(size);
    if (buffer == NULL) {
      caml_raise_out_of_memory();
    }

    ssize_t read = getxattr(String_val(path_v), String_val(name_v), buffer, size);
    if (read >= 0) {
      result = caml_alloc_initialized_string(read, buffer);
      free(buffer);
      CAMLreturn(result);
    }

    int error_code = errno;
    free(buffer);
    if (error_code == ERANGE) {
      size *= 2;
      continue;
    }
    if (e2e_xattr_missing(error_code)) {
      caml_raise_not_found();
    }
    caml_unix_error(error_code, "getxattr", path_v);
  }
#else
  caml_failwith("xattrs are not supported by this environment");
#endif
}

CAMLprim value caml_e2e_listxattr(value path_v) {
  CAMLparam1(path_v);
  CAMLlocal3(result, name, cell);
#if defined(__linux__)
  size_t size = 4096;
  while (1) {
    char *buffer = malloc(size);
    if (buffer == NULL) {
      caml_raise_out_of_memory();
    }

    ssize_t read = listxattr(String_val(path_v), buffer, size);
    if (read >= 0) {
      char *cursor = buffer;
      char *end = buffer + read;
      result = Val_emptylist;
      while (cursor < end) {
        size_t name_length = strlen(cursor);
        name = caml_copy_string(cursor);
        cell = caml_alloc(2, 0);
        Store_field(cell, 0, name);
        Store_field(cell, 1, result);
        result = cell;
        cursor += name_length + 1;
      }
      free(buffer);
      CAMLreturn(result);
    }

    int error_code = errno;
    free(buffer);
    if (error_code == ERANGE) {
      size *= 2;
      continue;
    }
    caml_unix_error(error_code, "listxattr", path_v);
  }
#else
  caml_failwith("xattrs are not supported by this environment");
#endif
}

CAMLprim value caml_e2e_removexattr(value path_v, value name_v) {
  CAMLparam2(path_v, name_v);
#if defined(__linux__)
  if (removexattr(String_val(path_v), String_val(name_v)) == -1) {
    int error_code = errno;
    if (e2e_xattr_missing(error_code)) {
      caml_raise_not_found();
    }
    caml_unix_error(error_code, "removexattr", path_v);
  }
  CAMLreturn(Val_unit);
#else
  caml_failwith("xattrs are not supported by this environment");
#endif
}
