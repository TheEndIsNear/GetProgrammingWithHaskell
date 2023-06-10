getRequestURL host apiKey resource id = host ++
                                        "/" ++
                                        resource ++
                                        "/" ++
                                        id ++
                                        "?token=" ++
                                        apiKey


exampleUrlBuilder = getRequestURL "http://example.com"

myExampleUrlBuider = exampleUrlBuilder "1337hAskell2"

genApiRequestBuilder = myExampleUrlBuider "1234"
