# yesod-hash-static
Extension to yesod-static.

Template haskell functions that allow you to transform the static file names produced so that they aren't 
identical to the underlying files.

Useful for applications like hash tagging file names.


# Example 

``` haskell

-- In a separate file
transformFileName :: Foldable t => t Char -> [Char]
transformFileName str = foldr (\c str' -> case c of
                                            'e' -> 'p':str'
                                            o -> o:str' ) "" str



-- call theabove to transform the name from e to o 
staticFilesListWithXForm "./" transformFileName ["example_with_83828.js"]

```

This generates 
`pxamplp_with_83828_js`
