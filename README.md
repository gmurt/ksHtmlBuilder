# ksHtmlBuilder
## Interface for building email compatible html templates

I'm not sure if others will find this useful or not, but I wrote it for one of my projects where I am going to allow users to create email templates from pre-defined blocks (rather than let them loose on raw HTML)

It Basically allows the creation of HTML documents from a Delphi interface class, this can then be saved and re-loaded from JSON.  

It will generate HTML by calling .AsHtml with an Email or Browser parameter.  If email is the desired output, all DIV tags will be converted to more email friendly TABLE tags and all CSS will be inlined on the objects themselves.

### Features

- Automatically in-lines css styles for better email client support
- Smart conversion of DIV tags (which are not 100% supported with email clients) into
- Supports pure HTML/Css bootstrap inspired Alerts & Buttons
- Loading/saving to/from JSON with support for standard TJsonObject and JsonDataObjects (via a conditional define


![image](https://user-images.githubusercontent.com/1161351/212573304-ea30ed7c-9240-4f94-9f7e-5bf10d9cff08.png)
