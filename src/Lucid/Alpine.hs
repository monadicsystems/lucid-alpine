{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Lucid.Alpine where

import Data.Text
import Lucid.Base (Attribute, makeAttribute)

-- | x-data
-- Declare a new Alpine component and its data for a block of HTML
xData_ :: Text -> Attribute
xData_ = makeAttribute "x-data"

{-
<div x-data="{ open: false }">
    ...
</div>
-}

-- | x-bind
-- Dynamically set HTML attributes on an element
xBind_
  :: Text -- ^ Attribute name
  -> Text
  -> Attribute
xBind_ attr = makeAttribute ("x-bind:" <> attr)

{-
<div x-bind:class="! open ? 'hidden' : ''">
  ...
</div>
-}

-- | x-on
-- Listen for browser events on an element
xOn_
  :: Text -- ^ Event name
  -> Text
  -> Attribute
xOn_ event = makeAttribute ("x-on:" <> event)

{-
<button x-on:click="open = ! open">
  Toggle
</button>
-}

-- | x-text
-- Set the text content of an element
xText_ :: Text -> Attribute
xText_ = makeAttribute "x-text"

{-
<div>
  Copyright ©

  <span x-text="new Date().getFullYear()"></span>
</div>
-}

-- | x-html
-- Set the inner HTML of an element
xHtml_ :: Text -> Attribute
xHtml_ = makeAttribute "x-html"

{-
<div x-html="(await axios.get('/some/html/partial')).data">
  ...
</div>
-}

-- | x-model
-- Synchronize a piece of data with an input element
xModel_
  :: [Text] -- ^ List of x-model modifiers
  -> Text
  -> Attribute
xModel_ mods = case mods of
  [] -> makeAttribute "x-model"
  _  -> makeAttribute ("x-model." <> intercalate "." mods)

{-
<div x-data="{ search: '' }">
  <input type="text" x-model="search">
 
  Searching for: <span x-text="search"></span>
</div>
-}

-- | x-show
-- Toggle the visibility of an element
xShow_ :: Text -> Attribute
xShow_ = makeAttribute "x-show"

{-
<div x-show="open">
  ...
</div>
-}

-- | x-transition
-- Transition an element in and out using CSS transitions
xTransition_
  :: Maybe Text -- ^ Transition directive
  -> [Text]     -- ^ List of x-transition modifiers
  -> Text
  -> Attribute
xTransition_ Nothing [] _ = makeAttribute "x-transition" mempty -- No directive or modifiers
xTransition_ (Just dir) [] attrVal = makeAttribute ("x-transition:" <> dir) attrVal -- Directive with custom transition classes
xTransition_ Nothing mods _ = makeAttribute ("x-transition." <> intercalate "." mods) mempty -- No directive, but with modifiers
xTransition_ (Just dir) mods _ = makeAttribute ("x-transition:" <> dir <> "." <> intercalate "." mods) mempty -- Directive with modifiers

{-
<div x-show="open" x-transition>
  ...
</div>
-}

-- | x-for
-- Repeat a block of HTML based on a data set
xFor_ :: Text -> Attribute
xFor_ = makeAttribute "x-for"

xForKey_ :: Text -> Attribute
xForKey_ = makeAttribute ":key"

{-
<template x-for="post in posts">
  <h2 x-text="post.title"></h2>
</template>
-}

-- | x-if
-- Conditionally add/remove a block of HTML from the page entirely.
xIf_ :: Text -> Attribute
xIf_ = makeAttribute "x-if"

{-
<template x-if="open">
  <div>...</div>
</template>
-}

-- | x-init
-- Run code when an element is initialized by Alpine
xInit_ :: Text -> Attribute
xInit_ = makeAttribute "x-init"

{-
<div x-init="date = new Date()"></div>
-}

-- | x-effect
-- Execute a script each time one of its dependancies change
xEffect_ :: Text -> Attribute
xEffect_ = makeAttribute "x-effect"

{-
<div x-effect="console.log('Count is '+count)"></div>
-}

-- | x-ref
-- Reference elements directly by their specified keys using the $refs magic property
xRef_ :: Text -> Attribute
xRef_ = makeAttribute "x-ref"

{-
<input type="text" x-ref="content">
 
<button x-on:click="navigator.clipboard.writeText($refs.content.value)">
  Copy
</button>
-}

-- | x-cloak
-- Hide a block of HTML until after Alpine is finished initializing its contents
xCloak_ :: Attribute
xCloak_ = makeAttribute "x-cloak" mempty

{-
<div x-cloak>
  ...
</div>
-}

-- | x-ignore
-- Prevent a block of HTML from being initialized by Alpine
xIgnore_ :: Attribute
xIgnore_ = makeAttribute "x-ignore" mempty

{-
<div x-ignore>
  ...
</div>
-}
