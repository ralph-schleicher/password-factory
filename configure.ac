dnl Process this file with autoconf to produce a configure script.

AC_PREREQ([2.71])

AC_INIT([password-factory],[1.0.2],[rs@ralph-schleicher.de])
AM_INIT_AUTOMAKE([foreign no-define no-dependencies])

AC_CONFIG_SRCDIR([packages.lisp])
AC_CONFIG_FILES([Makefile])

AC_CHECK_PROGS([SBCL], [sbcl], [true])
AC_CHECK_PROGS([CCL], [ccl], [true])
AC_CHECK_PROGS([OPENSSL], [openssl], [true])
AC_CHECK_PROGS([SYSTEMCTL], [systemctl], [true])
AC_CHECK_PROGS([ID], [id], [true])

dnl Optional features.
AC_PRESERVE_HELP_ORDER

ax_servicedir=
if test "x$ax_servicedir" = x && test -d /srv
then
    ax_servicedir=/srv
fi
if test "x$ax_servicedir" = x && test -d /var
then
    ax_servicedir=/var
fi
if test "x$ax_servicedir" = x
then
    ax_servicedir='$(libdir)'
fi
AC_MSG_CHECKING([for the service installation directory])
AC_MSG_RESULT([$ax_servicedir])
servicedir=$ax_servicedir
AC_SUBST([servicedir])

package_servicedir='$(servicedir)/$(PACKAGE_NAME)'
package_service_bindir='$(package_servicedir)/bin'
package_service_confdir='$(package_servicedir)/conf'
package_service_templdir='$(package_servicedir)/templ'
package_service_basedir='$(package_servicedir)/data'
AC_SUBST([package_servicedir])
AC_SUBST([package_service_bindir])
AC_SUBST([package_service_confdir])
AC_SUBST([package_service_templdir])
AC_SUBST([package_service_basedir])

ax_user=www-data
AC_ARG_WITH([user], AS_HELP_STRING([--with-user=NAME],[run service as user NAME [www-data]]),
[case $withval in
  yes | no)
    AC_MSG_ERROR([invalid value '$withval' for --with-user])
    ;;
  *)
    ax_user=$withval
    ;;
esac])
AC_MSG_CHECKING([for the user name of the service])
AC_MSG_RESULT([$ax_user])
USER=$ax_user
AC_SUBST([USER])

ax_group=www-data
AC_ARG_WITH([group], AS_HELP_STRING([--with-group=NAME],[run service as group NAME [www-data]]),
[case $withval in
  yes | no)
    AC_MSG_ERROR([invalid value '$withval' for --with-group])
    ;;
  *)
    ax_group=$withval
    ;;
esac])
AC_MSG_CHECKING([for the group name of the service])
AC_MSG_RESULT([$ax_group])
GROUP=$ax_group
AC_SUBST([GROUP])

ax_ssl_cert_file=example.pem
AC_ARG_WITH([ssl-cert-file], AS_HELP_STRING([--with-ssl-cert-file=NAME],[SSL certificate file name]),
[case $withval in
  [[\\/]]* | ?:[[\\/]]*) # absolute
    if test -f $withval
    then
	:
    else
	AC_MSG_ERROR([SSL certificate file '$withval' does not exist])
    fi
    ax_ssl_cert_file=$withval
    ;;
  *[[\\/]]* | yes | no)
    AC_MSG_ERROR([invalid value '$withval' for --with-ssl-cert-file])
    ;;
  *)
    ax_ssl_cert_file=$withval
    ;;
esac])
AC_MSG_CHECKING([for file name containing the SSL certificate])
AC_MSG_RESULT([$ax_ssl_cert_file])
ssl_cert_file=$ax_ssl_cert_file
ssl_cert_path='$(package_service_confdir)/ssl/$(ssl_cert_file)'
case $ax_ssl_cert_file in
  [[\\/]]* | ?:[[\\/]]*) # absolute
    ssl_cert_file=
    ssl_cert_path=$ax_ssl_cert_file
    ;;
esac
AC_SUBST([ssl_cert_file])
AC_SUBST([ssl_cert_path])

ax_ssl_key_file=example.key
AC_ARG_WITH([ssl-key-file], AS_HELP_STRING([--with-ssl-key-file=NAME],[SSL private key file name]),
[case $withval in
  [[\\/]]* | ?:[[\\/]]*) # absolute
    if test -f $withval
    then
	:
    else
	AC_MSG_ERROR([SSL private key file '$withval' does not exist])
    fi
    ax_ssl_key_file=$withval
    ;;
  *[[\\/]]* | yes | no)
    AC_MSG_ERROR([invalid value '$withval' for --with-ssl-key-file])
    ;;
  *)
    ax_ssl_key_file=$withval
    ;;
esac])
AC_MSG_CHECKING([for file name containing the SSL private key])
AC_MSG_RESULT([$ax_ssl_key_file])
ssl_key_file=$ax_ssl_key_file
ssl_key_path='$(package_service_confdir)/ssl/$(ssl_key_file)'
case $ax_ssl_key_file in
  [[\\/]]* | ?:[[\\/]]*) # absolute
    ssl_key_file=
    ssl_key_path=$ax_ssl_key_file
    ;;
esac
AC_SUBST([ssl_key_file])
AC_SUBST([ssl_key_path])

AM_MAINTAINER_MODE

AC_SUBST([PACKAGE_NAME])
AC_SUBST([PACKAGE_VERSION])
AC_SUBST([PACKAGE_STRING])
AC_SUBST([PACKAGE_BUGREPORT])
AC_SUBST([PACKAGE_TARNAME])

AC_OUTPUT

if $ID -u "$USER" > /dev/null 2>&1
then
    :
else
    AC_MSG_WARN([user '$USER' does not exist, try option --with-user])
fi
if $ID -g "$GROUP" > /dev/null 2>&1
then
    :
else
    AC_MSG_WARN([group '$GROUP' does not exist, try option --with-group])
fi
if test -f "$ax_ssl_cert_file" && test -f "$ax_ssl_key_file"
then
    :
else
    AC_MSG_WARN([consider running 'sh $srcdir/make-ssl-cert.sh'])
fi

dnl configure.ac ends here
