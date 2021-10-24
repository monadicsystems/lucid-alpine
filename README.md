# lucid-alpine
Lucid EDSL + Alpine.js = lucid-alpine

## Core

```haskell
import qualified Lucid as L
import qualified Data.Aeson as Aeson

{-
<div x-data="{ open: false }">
    <button @click="open = true">Open Dropdown</button>

    <ul
        x-show="open"
        @click.away="open = false"
    >
        Dropdown Body
    </ul>
</div>
-}

data DropdownState = DropdownState
  { open :: Bool
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)
  
initialDropdownState = DropdownState { open = False }

html1 = do
  div_ [x_data_ initialDropdownState, class_ "bg-red"] $ do
    (button_ [click@_ "open = true"] "Open Dropdown")
    (ul_ [x_show_ "open", clickAway@_ "open = false"] "Dropdown Body")
    
{-
<div x-data="{ tab: 'foo' }">
    <button :class="{ 'active': tab === 'foo' }" @click="tab = 'foo'">Foo</button>
    <button :class="{ 'active': tab === 'bar' }" @click="tab = 'bar'">Bar</button>

    <div x-show="tab === 'foo'">Tab Foo</div>
    <div x-show="tab === 'bar'">Tab Bar</div>
</div>
-}

data TabState = TabState
  { tab :: String
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

initialTabState = TabState { tab = "foo" }

html2 = do
    div_ [x_data_ TabState] $ do
        _button []
        _button []
        
        (_div [x_show_ "tab === 'foo'"] $ "Tab Foo")
        (_div [x_show_ "tab === 'bar'"] $ "Tab Bar")
```

Probably going to have to take away using Haskell records. Just make it a thin layer over Alpine. Could have a checking function like `alpine_` like in `lucid-tailwind`, but what would I check?
