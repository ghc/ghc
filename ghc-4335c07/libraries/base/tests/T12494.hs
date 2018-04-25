import System.Environment.Blank

main = do
  let envVar = "AN_ENVIRONMENT_VARIABLE"

  valueBeforeSettingVariable <- getEnv envVar
  print valueBeforeSettingVariable -- Nothing

  valueWithDefaultBeforeSetting <- getEnvDefault envVar "DEFAULT"
  print valueWithDefaultBeforeSetting -- "DEFAULT"

  setEnv envVar "" False

  valueAfterSettingVariable <- getEnv envVar
  print valueAfterSettingVariable -- Just ""

  valueWithDefaultAfterSetting <- getEnvDefault envVar "DEFAULT"
  print valueWithDefaultAfterSetting -- ""

  valueFromGetEnvironment <- lookup envVar <$> getEnvironment
  print valueFromGetEnvironment -- Just ""

  setEnv envVar "NO_OVERRIDE" False

  valueAfterSettingWithExistingValueAndOverrideFalse <- getEnv envVar
  print valueAfterSettingWithExistingValueAndOverrideFalse -- Just ""

  setEnv envVar "OVERRIDE" True

  valueAfterSettingWithExistingValueAndOverrideTrue <- getEnv envVar
  print valueAfterSettingWithExistingValueAndOverrideTrue -- Just "OVERRIDE"

  unsetEnv envVar

  valueAfterUnsettingVariable <- getEnv envVar
  print valueAfterUnsettingVariable -- Nothing
