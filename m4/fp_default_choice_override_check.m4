# FP_DEFAULT_CHOICE_OVERRIDE_CHECK(
#   flag, name, anti name, var name, help string,
#   [var true val], [var false val], [flag true val])
# ---------------------------------------------------
# Helper for when there is a automatic detection and an explicit flag for the
# user to override disable a feature, but not override enable a feature.
#
# $1 = flag of feature
# $2 = name of feature
# $3 = name of anti feature
# $4 = name of variable
# $5 = help string
# $6 = when true
# $7 = when false
# $8 = default explicit case (yes/no). Used for handle "backwards" legacy
#      options where enabling makes fewer assumptions than disabling.
AC_DEFUN([FP_DEFAULT_CHOICE_OVERRIDE_CHECK],
    [AC_ARG_ENABLE(
        [$1],
        [AS_HELP_STRING(
            [--enable-$1],
            [$5])],
        [AS_IF(
           [test x"$enableval" = x"m4_default([$8],yes)"],
           [AS_CASE(
               [x"$$4Default"],
               [x"m4_default([$6],YES)"],
                 [AC_MSG_NOTICE([user chose $2 matching default for platform])],
               [x"m4_default([$7],NO)"],
                 [AC_MSG_ERROR([user chose $2 overriding only supported option for platform])],
               [AC_MSG_ERROR([invalid default])])
            $4=m4_default([$6],YES)],
           [AS_CASE(
               [x"$$4Default"],
               [x"m4_default([$6],YES)"],
                 [AC_MSG_NOTICE([user chose $3 overriding for platform])],
               [x"m4_default([$7],NO)"],
                 [AC_MSG_NOTICE([user chose $3 matching default for platform])],
               [AC_MSG_ERROR([invalid default])])
            $4=m4_default([$7],NO)])],
        [$4="$$4Default"])])
