successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just val) = Just (val + 1)
incMaybe Nothing = Nothing

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe Nothing = Nothing
reverseMaybe (Just val) = Just (reverse val)

successStr :: Maybe String
successStr = show <$> successfulRequest

failedStr :: Maybe String
failedStr = show <$> failedRequest
