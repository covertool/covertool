-record(config, {appname = 'Application',
                 prefix_len = 0,
                 cover_data = no_import,
                 output = "coverage.xml",
                 summary = false,
                 sources = ["src/"],
                 beams = ["ebin/"],
                 lookup_source = no_callback :: no_callback | fun((module()) -> string() | false)
                }).
