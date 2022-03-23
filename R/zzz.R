pkgenv = new.env(parent = emptyenv())
pkgenv$latest_supported_version = '0.3.9'
pkgenv$valid_api_versions = c(
    '0.3.9',
    '0.3.8',
    '0.3.7',
    '0.3.6',
    '0.3.5',
    '0.3.4',
    '0.3.3',
    '0.3.2',
    '0.3.1',
    '0.3.0',
    '0.2.3',
    '0.2.2',
    '0.2.1',
    '0.2.0',
    '0.1.4',
    '0.1.3',
    '0.1.2',
    '0.1.1'
)

.onLoad = function(libname, pkgname) {
    op = options()
    latest_supported_version = pkgenv$latest_supported_version
    pkgops = list(
        halodotapi.default_version = latest_supported_version
    )
    to_set = ! names(pkgops) %in% names(op)
    if (any(to_set)) {
        options(pkgops[to_set])
    }
}

.onAttach = function(libname, pkgname) {
    set_version = getOption('halodotapi.default_version')
    packageStartupMessage("Halodotapi version is set to: ", set_version)
    latest_supported_version = pkgenv$latest_supported_version
    if (set_version %in% pkgenv$valid_api_versions) {
        if (set_version != latest_supported_version) {
            packageStartupMessage("Current supported version is ", latest_supported_version, "; package may be unstable on ", set_version, " until stable release of Halodotapi.")
        }
    } else {
        packageStartupMessage('Version is not supported! Please consider using ', latest_supported_version)
    }

    # Check existence of API token
    autocode_api_token = Sys.getenv("AUTOCODE_API_KEY")
    if (autocode_api_token == '') {
        packageStartupMessage("No Autocode API key found!")
        packageStartupMessage("Obtain one by navigating to https://autocode.com/ and registering for an account.
Go to your Profile > Account > Identity Tokens > General Use Identity Tokens and click \"Create General Use Identity\" ")
    }
}

