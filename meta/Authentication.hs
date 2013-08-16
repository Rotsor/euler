import Network.HTTP
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup

loginFormUrl = "http://projecteuler.net/login"

main = do
  loginForm <- simpleHTTP (getRequest loginFormUrl) >>= getResponseBody
  print $ tagTree . parseTags $ loginForm