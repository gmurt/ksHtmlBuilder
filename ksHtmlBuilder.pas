{ ******************************************************************************
*                                                                              *
*  ksHtmlBuilder                                                               *
*                                                                              *
*  https://github.com/gmurt/ksHtmlBuilder                                     *
*                                                                              *
*  Copyright 2023 Graham Murt                                                  *
*                                                                              *
*  email: graham@kernow-software.co.uk                                         *
*                                                                              *
*  Licensed under the Apache License, Version 2.0 (the "License");             *
*  you may not use this file except in compliance with the License.            *
*  You may obtain a copy of the License at                                     *
*                                                                              *
*    http://www.apache.org/licenses/LICENSE-2.0                                *
*                                                                              *
*  Unless required by applicable law or agreed to in writing, software         *
*  distributed under the License is distributed on an "AS IS" BASIS,           *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    *
*  See the License for the specific language governing permissions and         *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}

unit ksHtmlBuilder;

interface

{.$DEFINE USE_JSONDATAOBJECTS}


uses Windows, Classes, System.Generics.Collections, Graphics
  {$IFDEF USE_JSONDATAOBJECTS}
  , JsonDataObjects
  {$ELSE}
  , Json
  {$ENDIF}
  ;

type
  IHtmlDocument = interface;

  THtmlCssStyle = class;
  THtmlElement = class;
  THtmlRawElement = class;
  THtmlDivElement = class;
  THtmlHeaderElement = class;
  THtmlHrElement = class;
  THtmlBrElement = class;
  THtmlImageElement = class;
  THtmlParagraphElement = class;
  THtmlAlertElement = class;
  THtmlButtonElement = class;
  THtmlLinkElement = class;
  THtmlSocialIcons = class;

  THtmlElementClass = class of THtmlElement;

  THtmlRenderTarget = (htmlBrowser, htmlEmail);

  THtmlSection = (htmlBody,
                  htmlHead,
                  htmlHtml,
                  htmlScript,
                  htmlStyle);

  THtmlHeaderType = (h1, h2, h3, h4, h5, h6);

  THtmlCssAttribute = (
    cssBackground,
    cssBackgroundColor,
    cssBorder,
    cssBorderCollapse,
    cssBorderColor,
    cssBorderRadius,
    cssBorderStyle,
    cssBorderWidth,
    cssColor,
    cssCursor,
    cssDisplay,
    cssFontFamily,
    cssFontSize,
    cssFontStyle,
    cssFontWeight,
    cssHeight,
    cssLineHeight,
    cssMargin,
    cssMarginBottom,
    cssMarginTop,
    cssMaxHeight,
    cssMaxWidth,
    cssMinHeight,cssObjectFit,
    cssPadding,
    cssTextAlign,
    cssTextDecoration,
    cssVerticalAlign,
    cssWidth,
    cssWhiteSpace
  );

  THtmlTagAttribute = (attCellSpacing, attCellPadding, attHeight, attHref, attID, attSrc, attType, attWidth, attValue);

  THtmlButtonStyle = (btnPrimary, btnSecondary, btnSuccess, btnDanger, btnWarning, btnInfo, btnLight, btnDark, btnLink);
  THtmlAlertStyle = (asSuccess, asDanger, asWarning);


  THtmlSocialUrls = record
    FFacebookUrl: string;
    FInstagramUrl: string;
    FTwitterUrl: string;
  end;

  TCssAttributeList = class(TDictionary<THtmlCssAttribute, string>)
  private
    function GetAsSingleLine: string;
  public
    procedure SaveToJson(AJson: TJsonArray);
    procedure LoadFromJson(AJson: TJsonArray);
    property AsSingleLine: string read GetAsSingleLine;
  end;

  THtmlAttributeList = class(TDictionary<THtmlTagAttribute, string>)
  public
    procedure LoadFromJson(AArray: TJsonArray);
    procedure SaveToJson(AArray: TJsonArray);
  end;

  TCssStyleList = class(TObjectDictionary<string, THtmlCssStyle>)
  private
    function GetStyle(AName: string): THtmlCssStyle;
    procedure BuildDefaultStyles;
  public
    procedure LoadFromJson(AJson: TJsonArray);
    procedure SaveToJson(AJson: TJsonArray);
    procedure GetHtml(AStrings: TStrings);
    procedure SetAllHeaders(AAttribute: THtmlCssAttribute; AValue: string);
    property Style[AName: string]: THtmlCssStyle read GetStyle; default;
  end;


  THtmlElementList = class(TObjectList<THTmlElement>)
  private
    FOwner: THtmlElement;
    function AddImageStream(AStream: TStream): THtmlImageElement;
    function CreateElement(AObj: string): THtmlElement;
    function CreateClass(AClass: THtmlElementClass): THtmlElement;
    function GetElementByID(AID: string): THtmlElement;
  public
    constructor Create(AOwner: THtmlElement);
    
    function AddButton(AText, AUrl: string; AStyle: THtmlButtonStyle): THtmlButtonElement;
    function AddDiv: THtmlDivElement;
    function AddHtml(AHtml: string): THtmlRawElement;
    function AddSpacer(AHeight: integer): THtmlDivElement;
    function AddHeader(AType: THtmlHeaderType; AText: string): THtmlHeaderElement;
    function AddHr: THtmlHrElement;
    function AddBr: THtmlBrElement;

    function AddImage(AImg: TGraphic): THtmlImageElement; overload;
    function AddImageFromFile(AFilename: string): THtmlImageElement; overload;
    function AddImageFromUrl(ASrc: string; const AInline: Boolean = False): THtmlImageElement; overload;
    function AddParagraph(AText: string): THtmlParagraphElement;
    function AddAlert(AText: string; AAlertStyle: THtmlAlertStyle): THtmlAlertElement;

    function AddSocialIcons(ADetails: THtmlSocialUrls): THtmlSocialIcons;
    function AddLink(AText, AUrl: string): THtmlLinkElement;
    procedure LoadFromJson(AJson: TJsonArray);
    procedure SaveToJson(AJson: TJsonArray);
    property ElementByID[AID: string]: THtmlElement read GetElementByID;
  end;

  THtmlCssStyle = class
  private
    FAttributes: TCssAttributeList;
    function GetAttribute(AName: THtmlCssAttribute): string;
    procedure SetAttribute(AName: THtmlCssAttribute; const Value: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromJson(AJson: TJsonObject);
    procedure SaveToJson(AJson: TJsonObject);
    procedure GetHtml(AStrings: TStrings);
    procedure SetBorderAttributes(AColor, AWidth, AStyle: string);
    procedure SetFontAttributes(AFamily, AColor, ASize: string);
    property Attribute[AName: THtmlCssAttribute]: string read GetAttribute write SetAttribute;
  end;

  
  THtmlElement = class(TPersistent)
  private
    [weak] FDocument: IHtmlDocument;
    FParent: THTmlElement;
    FElements: THtmlElementList;
    FContent: string;
    FClass: TStrings;
    FAttributes: THtmlAttributeList;
    FStyles: TCssAttributeList;

    function GetStyle(AStyle: THtmlCssAttribute): string;
    function GetAttributesSingleLine(ATarget: THtmlRenderTarget): string;
    function GetStylesSingleLine(ATarget: THtmlRenderTarget): string;
    procedure SetStyle(AStyle: THtmlCssAttribute; const Value: string);
    function GetClassSingleLine: string;
    function GetAttribute(AAttribute: THtmlTagAttribute): string;
    procedure SetAttribute(AAttribute: THtmlTagAttribute; const Value: string);
  protected
    function GetTag(ATarget: THtmlRenderTarget): string; virtual; abstract;
    function HasClosingTag: Boolean; virtual;
    procedure GetHtml(AHtml: TStrings; ATarget: THtmlRenderTarget); virtual;
    procedure GetInternalHtml(AHtml: TStrings; ATarget: THtmlRenderTarget); virtual;
  public
    constructor Create; virtual;
    procedure Clear;
    procedure LoadFromJson(AJson: TJsonObject); virtual;
    procedure SaveToJson(AJson: TJsonObject); virtual;

    destructor Destroy; override;
    property Content: string read FContent write FContent;
    property Elements: THtmlElementList read FElements;
    property Attribute[AAttribute: THtmlTagAttribute]: string read GetAttribute write SetAttribute;
    procedure SetBorderAttributes(AColor, AWidth, AStyle: string);
    procedure SetFontAttributes(AFamily, AColor, ASize: string);
    property Style[AStyle: THtmlCssAttribute]: string read GetStyle write SetStyle;
    property CssClass: TStrings read FClass;
  end;

  THtmlRawElement = class(THtmlElement)
  protected
    function GetTag(ATarget: THtmlRenderTarget): string; override;
  end;


  THtmlDivElement = class(THtmlElement)
  protected
    function GetTag(ATarget: THtmlRenderTarget): string; override;
  end;

  THtmlHrElement = class(THtmlElement)
  protected
    function GetTag(ATarget: THtmlRenderTarget): string; override;
    function HasClosingTag: Boolean; override;
  end;

  THtmlBrElement = class(THtmlElement)
  protected
    function GetTag(ATarget: THtmlRenderTarget): string; override;
    function HasClosingTag: Boolean; override;
  end;

  THtmlImageElement = class(THtmlElement)
  private
    function GetSrc: string;
    procedure SetSrc(const Value: string);
  protected
    function GetTag(ATarget: THtmlRenderTarget): string; override;
    function HasClosingTag: Boolean; override;
    procedure GetHtml(AHtml: TStrings; ATarget: THtmlRenderTarget); override;
  public
    property Src: string read GetSrc write SetSrc;
  end;

  THtmlTextElement = class(THtmlElement)
  private
    function GetText: string;
    procedure SetText(const Value: string);
  protected
    property Text: string read GetText write SetText;
  end;

  THtmlHeaderElement = class(THtmlTextElement)
  private
    FHeaderType: THtmlHeaderType;
  protected
    function GetTag(ATarget: THtmlRenderTarget): string; override;
  public
    property HeaderType: THtmlHeaderType read FHeaderType write FHeaderType;
  end;

  THtmlParagraphElement = class(THtmlTextElement)
  protected
    function GetTag(ATarget: THtmlRenderTarget): string; override;
  end;

  THtmlAlertElement = class(THtmlTextElement)
  private
    FStyle: THtmlAlertStyle;
  protected
    function GetTag(ATarget: THtmlRenderTarget): string; override;
  public
    property Style: THtmlAlertStyle read FStyle write FStyle;
  end;

  THtmlLinkElement = class(THtmlTextElement)
  protected
    function GetTag(ATarget: THtmlRenderTarget): string; override;
  end;

  THtmlButtonElement = class(THtmlTextElement)
  protected
    function GetTag(ATarget: THtmlRenderTarget): string; override;
  end;

  THtmlSocialIcons = class(THtmlDivElement)
  private
    FSocialUrls: THtmlSocialUrls;
  protected
    procedure GetInternalHtml(AHtml: TStrings; ATarget: THtmlRenderTarget); override;
  public
    property SocialUrls: THtmlSocialUrls read FSocialUrls write FSocialUrls;
  end;

  THtmlHeadSection = class(THtmlElement)
  private
    FMetaData: TStrings;
    FCssStyles: TCssStyleList;
  protected
    function GetTag(ATarget: THtmlRenderTarget): string; override;
    procedure GetInternalHtml(AStrings: TStrings; ATarget: THtmlRenderTarget); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;

    procedure LoadFromJson(AJson: TJsonObject); override;
    procedure SaveToJson(AJson: TJsonObject); override;

    property MetaData: TStrings read FMetaData;
    property Styles: TCssStyleList read FCssStyles;
  end;

  THtmlBodySection = class(THtmlElement)
  protected
    function GetTag(ATarget: THtmlRenderTarget): string; override;
  end;

  IHtmlDocument = interface
    ['{6B7716EE-3A39-493F-89D4-60077631259E}']
    function GetHead: THtmlHeadSection;
    function GetHeaderBanner: THtmlImageElement;
    function GetAsJson: string;
    function GetContent: THtmlDivElement;
    function GetContainer: THtmlDivElement;
    function GetFooter: THtmlDivElement;
    function GetAsHtml(ATarget: THtmlRenderTarget): string;
    procedure SetAsJson(const Value: string);
    procedure LoadFromJson(AJson: TJsonObject);
    procedure SaveToJson(AJson: TJsonObject);

    procedure SaveHtmlToFile(AFilename: string; const AFormat: THtmlRenderTarget = htmlBrowser);
    procedure Clear;
    property Head: THtmlHeadSection read GetHead;
    property HeaderBanner: THtmlImageElement read GetHeaderBanner;
    property Content: THtmlDivElement read GetContent;
    property Footer: THtmlDivElement read GetFooter;
    property AsHtml[ATarget: THtmlRenderTarget]: string read GetAsHtml;
    property AsJson: string read GetAsJson write SetAsJson;

  end;


  function CreateHtmlDocument: IHtmlDocument;

implementation

{$R *.res}

uses SysUtils, Rtti, Net.HttpClient, System.NetEncoding, Jpeg, System.TypInfo;

const
  C_FACEBOOK_ICON_URL = 'https://user-images.githubusercontent.com/1161351/215111435-0983d7e8-4804-41d5-b82c-2161777abe20.png';
  C_INSTAGRAM_ICON_URL = 'https://user-images.githubusercontent.com/1161351/215112166-fb15ffe7-f6dd-4077-9efe-4d5a7ef1df7c.png';
  C_TWITTER_ICON_URL = 'https://user-images.githubusercontent.com/1161351/215112156-2a73e843-4345-441f-b5e1-91c8ac30083c.png';

type
  THtmlCssAttributeMap = TDictionary<THtmlCssAttribute,string>;

  THtmlDocument = class(TInterfacedObject, IHtmlDocument)
  private
    FHead: THtmlHeadSection;
    FBody: THtmlBodySection;
    function GetAsHtml(ATarget: THtmlRenderTarget): string;
    function GetContainer: THtmlDivElement;
    function GetContent: THtmlDivElement;
    function GetHead: THtmlHeadSection;
    function GetFooter: THtmlDivElement;
    function GetHeaderBanner: THtmlImageElement;
    function GetAsJson: string;
    procedure SetAsJson(const Value: string);
  protected
    procedure Clear;
    procedure SaveHtmlToFile(AFilename: string; const AFormat: THtmlRenderTarget = htmlBrowser);
    procedure LoadFromJson(AJson: TJsonObject);
    procedure SaveToJson(AJson: TJsonObject);
    property Head: THtmlHeadSection read GetHead;
    property HeaderBanner: THtmlImageElement read GetHeaderBanner;
    property Content: THtmlDivElement read GetContent;
    property Footer: THtmlDivElement read GetFooter;
    property AsHtml[ATarget: THtmlRenderTarget]: string read GetAsHtml;
    property AsJson: string read GetAsJson write SetAsJson;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;


var
  InternalHtmlCssAttributeMap: THtmlCssAttributeMap;


function CreateHtmlDocument: IHtmlDocument;
begin
  Result := THtmlDocument.Create;
end;

{ THtmlDivElement }

function HtmlSectionToString(AElement: THtmlElement): string;
begin
  Result := TRttiEnumerationType.GetName(AElement);
  Result := StringReplace(Result, 'html', '', []).ToLower;
end;

function HtmlTagStringToAttribute(AStr: string): THtmlTagAttribute;
begin                   
  AStr := AStr.ToLower;
  if Pos('att', AStr) <> 1 then
    AStr := 'att' + AStr;
  
  Result := THtmlTagAttribute(GetEnumValue(TypeInfo(THtmlTagAttribute), aStr));;
end;

function HtmlTagAttributeToString(AAttribute: THtmlTagAttribute): string;
begin
  Result := StringReplace(TRttiEnumerationType.GetName(AAttribute), 'att', '', [rfReplaceAll]);
  Result := Result.ToLower;             
end;

function CssAttributeNameToString(AStyle: THtmlCssAttribute): string;
begin
  Result := '';
  InternalHtmlCssAttributeMap.TryGetValue(AStyle, Result);
end;

function CssAttributeNameFromString(AStr: string): THtmlCssAttribute;
begin
  AStr := AStr.ToLower;
  AStr := StringReplace(AStr, '-', '', [rfReplaceAll]);
  if Pos('css', AStr) <> 1 then
    AStr := 'css' + AStr;
  Result := THtmlCssAttribute(GetEnumValue(TypeInfo(THtmlCssAttribute), aStr));
end;

function ButtonStyleToString(AStyle: THtmlButtonStyle): string;
begin
  Result := LowerCase(StringReplace(TRttiEnumerationType.GetName(AStyle), 'btn', 'btn-', [rfIgnoreCase]));
end;

function AlertStyleToString(AStyle: THtmlAlertStyle): string;
begin
  Result := '';
  case AStyle of
    asSuccess: Result := 'alert-success';
    asDanger: Result := 'alert-danger';
    asWarning: Result := 'alert-warning';
  end;
end;

function THtmlDivElement.GetTag(ATarget: THtmlRenderTarget): string;
begin
  Result := 'div';
  if ATarget = htmlEmail then
    Result := 'table';
end;

{ THtmlElement }

procedure THtmlElement.Clear;
begin
  FElements.Clear;
  FContent := '';
  FClass.Clear;
  FAttributes.Clear;
  FStyles.Clear;
end;

constructor THtmlElement.Create;
begin
  inherited Create;
  FElements := THtmlElementList.Create(Self);
  FClass := TStringList.Create;
  FAttributes := THtmlAttributeList.Create;
  FStyles := TCssAttributeList.Create;
end;

destructor THtmlElement.Destroy;
begin
  FElements.Free;
  FAttributes.Free;
  FStyles.Free;
  FClass.Free;
  inherited;
end;

procedure THtmlElement.GetHtml(AHtml: TStrings; ATarget: THtmlRenderTarget);
var
  AElement: THTmlElement;
  ABlock: string;
  _ATag: string;
  APadding: string;
  ACellCss: string;

begin
  _ATag := GetTag(ATarget);
  if (ATarget = htmlEmail) and (Self is THtmlDivElement) then
  begin
    _ATag := 'table';
    Attribute[attCellSpacing] := '0';
    Attribute[attCellPadding] := '0';
    Style[cssBorderCollapse] := 'collapse';
    Style[cssWidth] := '100%';
  end;

  if _ATag <> '' then
  begin
    ABlock := '<'+_ATag;

    ABlock := Trim(ABlock + ' '+GetClassSingleLine);
    ABlock := Trim(ABlock + ' '+GetAttributesSingleLine(ATarget));
    ABlock := Trim(ABlock + ' '+GetStylesSingleLine(ATarget));
    ABlock := ABlock + '>';

    if _ATag = 'table' then
    begin
      ACellCss := '';
      APadding := Style[cssPadding];
      if APadding <> '' then ACellCss := 'style="padding:'+APadding+';"';
      ABlock := ABlock+'<tr><td valign="top"'+ACellCss+'>';
    end;
    AHtml.Add(ABlock);
  end;


  if Trim(FContent) <> '' then
  begin
    AHtml.Add(Trim(FContent));
  end;

  GetInternalHtml(AHtml, ATarget);

  for AElement in Elements do
  begin
    AElement.GetHtml(AHtml, ATarget);
  end;
  if HasClosingTag then
  begin

    ABlock := '</'+_ATag+'>';
    if _ATag = 'table' then ABlock := '</td></tr>'+ABlock;

    AHtml.Add(ABlock);
  end;
end;

procedure THtmlElement.GetInternalHtml(AHtml: TStrings; ATarget: THtmlRenderTarget);
begin
  // overridden in descendant classes
end;

function THtmlElement.GetStyle(AStyle: THtmlCssAttribute): string;
begin
  FStyles.TryGetValue(AStyle, Result);
end;

function THtmlElement.GetAttribute(AAttribute: THtmlTagAttribute): string;
begin
  FAttributes.TryGetValue(AAttribute, Result);
end;

function THtmlElement.GetAttributesSingleLine(ATarget: THtmlRenderTarget): string;
var
  ATagAttribute: THtmlTagAttribute;
begin
  Result := '';

  if FAttributes.Count > 0 then
  begin
    for ATagAttribute in FAttributes.Keys do
    begin
      if FAttributes[ATagAttribute] <> '' then
      begin
        Result := Trim(Result + ' '+HtmlTagAttributeToString(ATagAttribute)+'="'+FAttributes.Items[ATagAttribute]+'" ');
      end;
    end;
  end;
end;

function THtmlElement.GetClassSingleLine: string;
var
  AClass: string;
begin
  if FClass.Count > 0 then
  begin
    Result := 'class="';
    for AClass in FClass do
      Result := Result + StringReplace(AClass, '.', '', [])+' ';
    Result := Trim(Result) + '" ';
  end;
end;

function THtmlElement.GetStylesSingleLine(ATarget: THtmlRenderTarget): string;
var
  //AStyle: THtmlCssAttribute;
  AStyleStrings: TSTrings;
  AClasses: TStrings;
  //AClassStyle: THtmlCssStyle;

begin
  Result := '';

  AStyleStrings := TStringList.Create;
  AClasses := TStringList.Create;
  try
    if ATarget = htmlEmail then
    begin
      // extract css styles from stylesheet to inline on element for email output

      AClasses.Add(GetTag(htmlBrowser));
      for var s in FClass do
      begin
        AClasses.Add(s);
        AClasses.Add('.'+s);
      end;

      for var AClass in AClasses do
      begin
        var AClassStyle := FDocument.Head.FCssStyles.GetStyle(AClass);
        if AClassStyle <> nil then
        begin
          for var AStyle in AClassStyle.FAttributes.Keys do
          begin
            AStyleStrings.Values[CssAttributeNameToString(AStyle)] := AClassStyle.FAttributes.Items[AStyle];
          end;
        end;
      end;
    end;


    for var AStyle in FStyles.Keys do
    begin
      if FStyles[AStyle] <> '' then
      begin
        AStyleStrings.Values[CssAttributeNameToString(AStyle)] := FStyles.Items[AStyle];
      end;
    end;

    if AStyleStrings.Count > 0 then
    begin
      Result := Result + ' style="';
      for var ICount := 0 to AStyleStrings.Count-1 do
      begin
        Result := Trim(Result + ' '+AStyleStrings.Names[ICount]+': '+AStyleStrings.ValueFromIndex[ICount]+';');
      end;
      Result := Result + '" ';
    end;

  finally
    AClasses.Free;
    AStyleStrings.Free;
  end;
end;

function THtmlElement.HasClosingTag: Boolean;
begin
  Result := GetTag(htmlBrowser) <> '';
end;

procedure THtmlElement.LoadFromJson(AJson: TJsonObject);
begin
  {$IFDEF USE_JSONDATAOBJECTS}
  FClass.CommaText := AJson.S['class'];
  FAttributes.LoadFromJson(AJson.A['attributes']); 
  FStyles.LoadFromJSon(AJson.A['_styles']);
  
  FContent := AJson.S['content'];
  FElements.LoadFromJson(AJson.A['elements']);
  {$ELSE}
  if AJson.FindValue('class') <> nil then FClass.CommaText := AJson.Values['class'].Value;
  if AJson.FindValue('attributes') <> nil then FAttributes.LoadFromJson(AJson.Values['attributes'].AsType<TJSONArray>);
  if AJson.FindValue('_styles') <> nil then FStyles.LoadFromJson(AJson.Values['_styles'].AsType<TJSONArray>);
  if AJson.FindValue('content') <> nil then FContent := AJson.Values['content'].Value;
  if AJson.FindValue('elements') <> nil then FElements.LoadFromJson(AJson.Values['elements'].AsType<TJSONArray>);
  {$ENDIF}
end;

procedure THtmlElement.SaveToJson(AJson: TJsonObject);
{$IFNDEF USE_JSONDATAOBJECTS}
var
  AArray: TJSONArray;
{$ENDIF}
begin
  {$IFDEF USE_JSONDATAOBJECTS}
  AJson.S['obj'] :=  ClassName;
  if FClass.Count > 0 then AJson.S['class'] := FClass.CommaText;
  if FAttributes.Count > 0 then FAttributes.SaveToJson(AJson.A['attributes']);
  if FStyles.Count > 0 then FStyles.SaveToJson(AJson.A['_styles']);
    
  if FContent <> '' then AJson.S['content'] := FContent;

  
  if FElements.Count > 0 then
    FElements.SaveToJson(AJson.A['elements']);
  {$ELSE}

  AJson.AddPair('obj', ClassName);
  if FClass.Count > 0 then AJson.AddPair('class', FClass.CommaText);
  if FContent <> '' then AJson.AddPair('content', FContent);

  if FAttributes.Count > 0 then
  begin
    AArray := TJSONArray.Create;
    FAttributes.SaveToJson(AArray);
    AJson.AddPair('attributes', AArray);
  end;

  if FStyles.Count > 0 then
  begin
    AArray := TJSONArray.Create;
    FStyles.SaveToJson(AArray);
    AJson.AddPair('_styles', AArray);
  end;

  if FElements.Count > 0 then
  begin
    AArray := TJSONArray.Create;
    FElements.SaveToJson(AArray);
    AJson.AddPair('elements', AArray);
  end;
  {$ENDIF}
end;

procedure THtmlElement.SetAttribute(AAttribute: THtmlTagAttribute; const Value: string);
begin
  FAttributes.AddOrSetValue(AAttribute, Value);
end;

procedure THtmlElement.SetBorderAttributes(AColor, AWidth, AStyle: string);
begin
  if AColor <> '' then Style[cssBorderColor] := AColor;
  if AWidth <> '' then Style[cssBorderWidth] := AWidth;
  if AStyle <> '' then Style[cssBorderStyle] := AStyle;
end;

procedure THtmlElement.SetFontAttributes(AFamily, AColor, ASize: string);
begin
  if AFamily <> '' then Style[cssFontFamily] := AFamily;
  if AColor <> '' then Style[cssColor] := AColor;
  if ASize <> '' then Style[cssFontSize] := ASize;
end;

procedure THtmlElement.SetStyle(AStyle: THtmlCssAttribute; const Value: string);
begin
  FStyles.AddOrSetValue(AStyle, Value);
end;

{ THtmlHeadSection }

procedure THtmlHeadSection.Clear;
begin
  FCssStyles.Clear;
  FCssStyles.BuildDefaultStyles;
end;

constructor THtmlHeadSection.Create;
begin
  inherited;
  FMetaData := TStringList.Create;
  FMetaData.Add('<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />');
  FMetaData.Add('<meta http-equiv="X-UA-Compatible" content="IE=EmulateIE9" />');
  FMetaData.Add('<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0">');

  FCssStyles := TCssStyleList.Create([doOwnsValues]);
  FCssStyles.BuildDefaultStyles;
end;

destructor THtmlHeadSection.Destroy;
begin
  FCssStyles.Free;
  FMetaData.Free;
  inherited;
end;

procedure THtmlHeadSection.GetInternalHtml(AStrings: TStrings; ATarget: THtmlRenderTarget);
begin
  inherited;
  AStrings.AddStrings(FMetaData);
  if ATarget <> htmlEmail then
  begin
    FCssStyles.GetHtml(AStrings);
  end;
end;

function THtmlHeadSection.GetTag(ATarget: THtmlRenderTarget): string;
begin
  Result := 'head';
end;

procedure THtmlHeadSection.LoadFromJson(AJson: TJsonObject);
{$IFNDEF USE_JSONDATAOBJECTS}
var
  AArray: TJsonArray;
{$ENDIF}
begin
  inherited;
  {$IFDEF USE_JSONDATAOBJECTS}
  FCssStyles.loadFromJson(AJson.A['styles']);
  {$ELSE}
  AArray := AJson.GetValue('styles').AsType<TJSONArray>;
  FCssStyles.LoadFromJson(AArray);
  {$ENDIF}
end;

procedure THtmlHeadSection.SaveToJson(AJson: TJsonObject);
{$IFNDEF USE_JSONDATAOBJECTS}
var
  AArray: TJSONArray;
{$ENDIF}
begin
  inherited;
  {$IFDEF USE_JSONDATAOBJECTS}
  FCssStyles.SaveToJson(AJson.A['styles']);
  {$ELSE}
  AArray := TJSONArray.Create;
  FCssStyles.SaveToJson(AArray);
  AJson.AddPair('styles', AArray);
  {$ENDIF}
end;

{ THtmlDocument }

procedure THtmlDocument.Clear;
begin
  FHead.Clear;
  FBody.Clear;
end;

constructor THtmlDocument.Create;
var
  AContainer: THtmlDivElement;
  AContent: THtmlDivElement;
  ABanner: THtmlImageElement;
begin

  inherited Create;
  FHead := THtmlHeadSection.Create;
  FHead.FDocument := Self;
  FHead.FParent := nil;

  FBody := THtmlBodySection.Create;
  FBody.FDocument := Self;
  FBody.FParent := nil;

  fBody.Style[cssBackgroundColor] := ' #eaeaea';
  fBody.Style[cssPadding] := '12px';

  AContainer := FBody.Elements.AddDiv;
  AContainer.Attribute[attId] := '_container';
  AContainer.FClass.Add('container');
  AContainer.Style[cssMargin] := '0 auto';
  AContainer.SetBorderAttributes('#a6a6a6', '1px', 'solid');
  AContainer.Style[cssBackgroundColor] := 'white';
  AContainer.Style[cssMaxWidth] := '600px';
  AContainer.Style[cssMinHeight] := '50px';
  AContainer.Style[cssTextAlign] := 'center';

  ABanner := AContainer.Elements.AddImageFromUrl('');
  ABanner.Style[cssWidth] := '100%';
  ABanner.Attribute[attID] := '_headerBanner';

  AContent := AContainer.Elements.AddDiv;
  AContent.FClass.Add('content');
  AContent.Attribute[attId] := '_content';
  

  AContent.Style[cssPadding] := '24px';
  FBody.Elements.AddSpacer(20);

  FBody.Elements.AddDiv.Attribute[attID] := '_footer';
  
end;

destructor THtmlDocument.Destroy;
begin
  FHead.Free;
  FBody.Free;
  inherited;
end;


function THtmlDocument.GetAsHtml(ATarget: THtmlRenderTarget): string;
var
  AStrings: TStrings;
begin
  AStrings := TStringList.Create;
  try
    AStrings.Add('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">');
    AStrings.Add('<html>');
    FHead.GetHtml(AStrings, ATarget);
    FBody.GetHtml(AStrings, ATarget);
    AStrings.Add('</html>');
    Result := AStrings.Text;
  finally
    AStrings.Free;
  end;
end;
        
function THtmlDocument.GetAsJson: string;
var
  AJson: TJSONObject;
begin
  AJson := TJSONObject.Create;
  try
    SaveToJson(AJson);
    Result := AJson.ToJSON;
  finally
    AJson.Free;
  end;
end;

function THtmlDocument.GetContainer: THtmlDivElement;
begin
  Result := FBody.Elements.ElementByID['_container'] as THtmlDivElement;
end;

function THtmlDocument.GetContent: THtmlDivElement;
begin
  Result := GetContainer.Elements.ElementByID['_content'] as THtmlDivElement;
end;

function THtmlDocument.GetFooter: THtmlDivElement;
begin
  Result := FBody.Elements.ElementByID['_footer'] as THtmlDivElement;
end;

function THtmlDocument.GetHead: THtmlHeadSection;
begin
  Result := FHead;
end;

function THtmlDocument.GetHeaderBanner: THtmlImageElement;
begin
  Result := GetContainer.Elements.GetElementByID('_headerBanner') as THtmlImageElement;
end;         

procedure THtmlDocument.SaveHtmlToFile(AFilename: string; const AFormat: THtmlRenderTarget = htmlBrowser);
var
  AStrings: TStrings;
begin
  AStrings := TStringList.Create;
  try
    AStrings.Text := GetAsHtml(AFormat);
    AStrings.SaveToFile(AFilename);
  finally
    AStrings.Free;
  end;
end;

procedure THtmlDocument.SaveToJson(AJson: TJsonObject);
{$IFNDEF USE_JSONDATAOBJECTS}
var
  AHead, ABody: TJSONObject;
{$ENDIF}
begin
  {$IFDEF USE_JSONDATAOBJECTS}
  FHead.SaveToJson(AJson.O['head']);
  FBody.SaveToJson(AJson.O['body']);
  {$ELSE}
  AHead := TJSONObject.Create;
  FHead.SaveToJson(AHead);
  AJson.AddPair('head', AHead);
  ABody := TJSONObject.Create;
  FBody.SaveToJson(ABody);
  AJson.AddPair('body', ABody);
  {$ENDIF}
end;

procedure THtmlDocument.SetAsJson(const Value: string);
var
  AJson: TJsonObject;
begin
  {$IFDEF USE_JSONDATAOBJECTS}
  AJson := TJSONObject.Parse(Value) as TJSONObject;
  {$ELSE}
  AJson := TJsonObject.ParseJSONValue(Value) as TJSONObject;
  {$ENDIF}
  try
    LoadFromJson(AJson);
  finally
    AJson.Free;
  end;
end;

procedure THtmlDocument.LoadFromJson(AJson: TJsonObject);
begin
  Clear;
  {$IFDEF USE_JSONDATAOBJECTS}
  FHead.LoadFromJson(AJson.O['head']);
  FBody.LoadFromJson(AJson.O['body']);
  {$ELSE}
  FHead.LoadFromJson(AJson.GetValue('head') as TJSONObject);
  FBody.LoadFromJson(AJson.GetValue('body') as TJSONObject);
  {$ENDIF}
end;

{ THtmlBodySection }

function THtmlBodySection.GetTag(ATarget: THtmlRenderTarget): string;
begin
  Result := 'body';
end;

{ THtmlElementList }

function THtmlElementList.CreateClass(AClass: THtmlElementClass): THtmlElement;
begin
  Result := AClass.Create;
  Result.FDocument := FOwner.FDocument;
  Result.FParent := FOwner;
end;

function THtmlElementList.AddButton(AText, AUrl: string; AStyle: THtmlButtonStyle): THtmlButtonElement;
begin
  Result := CreateClass(THtmlButtonElement) as THtmlButtonElement;
  Result.Attribute[attHref] := AUrl;
  Result.CssClass.Add('btn');
  Result.CssClass.Add(ButtonStyleToString(AStyle));
  Result.Style[cssDisplay] := 'block';
  Result.Style[cssMarginTop] := '8px';
  Result.Style[cssMarginBottom] := '8px';
  Result.Text := AText;
  Add(Result);
end;

function THtmlElementList.AddDiv: THtmlDivElement;
begin
  Result := CreateClass(THtmlDivElement) as THtmlDivElement;
  Add(Result);
end;

function THtmlElementList.AddHeader(AType: THtmlHeaderType; AText: string): THtmlHeaderElement;
begin
  Result := CreateClass(THtmlHeaderElement) as THtmlHeaderElement;
  Result.HeaderType := AType;
  Result.Text := AText;
  Add(Result);
end;

function THtmlElementList.AddHr: THtmlHrElement;
begin
  Result := CreateClass(THtmlHrElement) as THtmlHrElement;
  Result.Style[cssBorder] := 'none';
  Result.Style[cssHeight] := '1px';
  Result.Style[cssBackgroundColor] := '#ccc';
  Add(Result);
end;

function THtmlElementList.AddHtml(AHtml: string): THtmlRawElement;
begin
  Result := CreateClass(THtmlRawElement) as THtmlRawElement;
  Result.Content := AHtml;
  Add(Result);
end;

function THtmlElementList.AddAlert(AText: string; AAlertStyle: THtmlAlertStyle): THtmlAlertElement;
begin
  Result := CreateClass(THtmlAlertElement) as THtmlAlertElement;
  Result.Style := AAlertStyle;
  Result.Text := AText;
  Result.FClass.Add('alert');
  Result.FClass.Add(AlertStyleToString(AAlertStyle));
  Add(Result);
end;

function THtmlElementList.AddBr: THtmlBrElement;
begin
  Result := CreateClass(THtmlBrElement) as THtmlBrElement;
  Add(Result);
end;

function THtmlElementList.AddImageStream(AStream: TStream): THtmlImageElement;
var
  AEncoded: TStringStream;
begin
  Result := CreateClass(THtmlImageElement) as THtmlImageElement;
  AEncoded := TStringStream.Create;
  try
    TNetEncoding.Base64.Encode(AStream, AEncoded);
    Result.Attribute[attSrc] := 'data:image/png;base64, '+AEncoded.DataString;
  finally
    AEncoded.Free;
  end;
  Add(Result);

end;

function THtmlElementList.AddLink(AText, AUrl: string): THtmlLinkElement;
begin
  Result := CreateClass(THtmlLinkElement) as THtmlLinkElement;
  Result.Text := AText;
  Result.Attribute[attHref] := AUrl;
  Add(Result);
end;

function THtmlElementList.AddParagraph(AText: string): THtmlParagraphElement;
begin
  Result := CreateClass(THtmlParagraphElement) as THtmlParagraphElement;
  Result.Text := AText;
  Add(Result);
end;

function THtmlElementList.AddSocialIcons(ADetails: THtmlSocialUrls): THtmlSocialIcons;
begin
  Result := CreateClass(THTmlSocialIcons) as THtmlSocialICons;
  Result.SocialUrls := ADetails;
  Add(Result);
end;

function THtmlElementList.AddSpacer(AHeight: integer): THtmlDivElement;
begin
  Result := AddDiv;
  Result.Style[cssHeight] := AHeight.ToString+'px';
  Result.Style[cssMargin] := '0px';
  Result.Style[cssPadding] := '0px';
end;

function THtmlElementList.AddImage(AImg: TGraphic): THtmlImageElement;
var
  ABmp: TBitmap;
  AJpg: TJPEGImage;
  AStream: TMemoryStream;
begin
  ABmp := TBitmap.Create;
  AJpg := TJPEGImage.Create;
  AStream := TMemoryStream.Create;
  try
    ABmp.SetSize(AImg.Width, AImg.Height);
    ABmp.Canvas.Draw(0, 0, AImg);
    AJpg.Assign(ABmp);
    AJpg.SaveToStream(AStream);
    AStream.Position := 0;
    Result := AddImageStream(AStream);
  finally
    ABmp.Free;
    AJpg.Free;
    AStream.Free;
  end;
end;

function THtmlElementList.AddImageFromUrl(ASrc: string; const AInline: Boolean = False): THtmlImageElement;
var
  AHttp: THTTPClient;
  AStream: TStream;
begin
  if AInline = False then
  begin
    Result := CreateClass(THtmlImageElement) as THtmlImageElement;
    Result.Attribute[attSrc] := ASrc;
    Add(Result);
  end
  else
  begin
    AHttp := THTTPClient.Create;
    AStream := TMemoryStream.Create;
    try
      AHttp.Get(ASrc, AStream);
      AStream.Position := 0;
      Result := AddImageStream(AStream);
    finally
      AHttp.Free;
      AStream.Free;
    end;
  end;
end;

function THtmlElementList.AddImageFromFile(AFilename: string): THtmlImageElement;
var
  AImg: TPicture;
begin
  AImg := TPicture.Create;
  try
    AImg.LoadFromFile(AFilename);
    Result := AddImage(AImg.Graphic);
  finally
    AImg.Free;
  end;
end;

constructor THtmlElementList.Create(AOwner: THtmlElement);
begin
  inherited Create(True);
  FOwner := AOwner;
end;

function THtmlElementList.CreateElement(AObj: string): THtmlElement;
var
  AClassRef: TClass;
  AElement: TObject;
begin
  Result := nil;
  AClassRef := GetClass(AObj);
  if AClassRef <> nil then
  begin
    AElement := AClassRef.Create;
    try
      if (AElement is THtmlElement) then
        Result := CreateClass(THtmlElementClass(AElement.ClassType));
    finally
      AElement.Free;
    end;
  end
  {$IFDEF DEBUG}
  else
    raise Exception.Create('Class not registered: '+AObj);
  {$ENDIF}
end;

function THtmlElementList.GetElementByID(AID: string): THtmlElement;
var
  AElement: THtmlElement;
begin
  Result := nil;
  for AElement in Self do
  begin
    if AElement.Attribute[attID] = AID then
    begin
      Result := AElement;
      Exit;
    end;
  end;
end;

procedure THtmlElementList.LoadFromJson(AJson: TJsonArray);
var
  {$IFNDEF USE_JSONDATAOBJECTS}
  AValue: TJSONValue;
  {$ELSE}
  AObj: TJsonObject;
  {$ENDIF}
  AElement: THtmlElement;
begin
  {$IFDEF USE_JSONDATAOBJECTS}
  for AObj in AJson do
  begin
    AElement := CreateElement(AObj.S['obj']);
    if AElement <> nil then
    begin
      AElement.FParent := FOwner;
      AElement.LoadFromJson(AObj);
      Add(AElement);
    end;
  end;
  {$ELSE}
  for AValue in AJson do
  begin
    AElement := CreateElement(AValue.AsType<TJsonOBject>.Values['obj'].Value);
    if AElement <> nil then
    begin
      AElement.FParent := FOwner;
      AElement.LoadFromJson(AValue.AsType<TJSONObject>);
      Add(AElement);
    end;
  end;

  {$ENDIF}
end;

procedure THtmlElementList.SaveToJson(AJson: TJsonArray);
var
  AElement: THtmlElement;
  AObj: TJsonObject;
begin
  for AElement in Self do
  begin
    AObj := TJsonObject.Create;
    AElement.SaveToJson(AObj);
    AJson.Add(AObj);
  end;
end;

{ THtmlHeaderElement }

function THtmlHeaderElement.GetTag(ATarget: THtmlRenderTarget): string;
begin
  Result := TRttiEnumerationType.GetName(FHeaderType);
end;

{ THtmlTextElement }

function THtmlTextElement.GetText: string;
begin
  Result := FContent;
end;

procedure THtmlTextElement.SetText(const Value: string);
begin
  FContent := Value;
end;

{ THtmlHrElement }

function THtmlHrElement.GetTag(ATarget: THtmlRenderTarget): string;
begin
  Result := 'hr';
end;

function THtmlHrElement.HasClosingTag: Boolean;
begin
  Result := False;
end;

{ THtmlImageElement }

procedure THtmlImageElement.GetHtml(AHtml: TStrings; ATarget: THtmlRenderTarget);
begin
  if Src = '' then
    Style[cssDisplay] := 'none';
  inherited;
end;

function THtmlImageElement.GetSrc: string;
begin
  Result := Attribute[THtmlTagAttribute.attSrc];
end;

function THtmlImageElement.GetTag(ATarget: THtmlRenderTarget): string;
begin
  Result := 'img';
end;

function THtmlImageElement.HasClosingTag: Boolean;
begin
  Result := False;
end;

procedure THtmlImageElement.SetSrc(const Value: string);
begin
  Attribute[THtmlTagAttribute.attSrc] := Value;
end;

{ THtmlBrElement }

function THtmlBrElement.GetTag(ATarget: THtmlRenderTarget): string;
begin
  Result := 'br';
end;

function THtmlBrElement.HasClosingTag: Boolean;
begin
  Result := False;
end;

{ TCssStyle }

constructor THtmlCssStyle.Create;
begin
  inherited Create;
  FAttributes := TCssAttributeList.Create;
end;

destructor THtmlCssStyle.Destroy;
begin
  FAttributes.Free;
  inherited;
end;

function THtmlCssStyle.GetAttribute(AName: THtmlCssAttribute): string;
begin
  Result := '';
  FAttributes.TryGetValue(AName, Result);
end;

procedure THtmlCssStyle.GetHtml(AStrings: TStrings);
var
  AAttribute: THtmlCssAttribute;
begin
  for AAttribute in FAttributes.Keys do
  begin

  end;
end;

procedure THtmlCssStyle.LoadFromJson(AJson: TJsonObject);
{$IFNDEF USE_JSONDATAOBJECTS}
var
  AArray: TJSONArray;
{$ENDIF}
begin
  {$IFDEF USE_JSONDATAOBJECTS}  
  FAttributes.LoadFromJSon(AJson.A['attributes']);
  {$ELSE}
  if AJson.FindValue('attributes') <> nil then 
  begin
    AArray := AJson.GetValue('attributes').AsType<TJSONArray>;
    FAttributes.LoadFromJson(AArray);
  end;
  {$ENDIF}
end;

procedure THtmlCssStyle.SaveToJson(AJson: TJsonObject);
{$IFNDEF USE_JSONDATAOBJECTS}
var
  AArray: TJSONArray;
{$ENDIF}
begin
  {$IFDEF USE_JSONDATAOBJECTS}
  FAttributes.SaveToJson(AJson.A['attributes']);
  {$ELSE}
  AArray := TJSONArray.Create;

  FAttributes.SaveToJson(AArray);
  AJson.AddPair('attributes', AArray);

  {$ENDIF}
end;

procedure THtmlCssStyle.SetAttribute(AName: THtmlCssAttribute; const Value: string);
begin
  FAttributes.AddOrSetValue(AName, StringReplace(Value, ';', '', [rfReplaceAll]));
end;

procedure THtmlCssStyle.SetFontAttributes(AFamily, AColor, ASize: string);
begin
  if AFamily <> '' then Attribute[cssFontFamily] := AFamily;
  if AColor <> '' then Attribute[cssColor] := AColor;
  if ASize <> '' then Attribute[cssFontSize] := ASize;
end;

procedure THtmlCssStyle.SetBorderAttributes(AColor, AWidth, AStyle: string);
begin
  if AColor <> '' then Attribute[cssBorderColor] := AColor;
  if AWidth <> '' then Attribute[cssBorderWidth] := AWidth;
  if AStyle <> '' then Attribute[cssBorderStyle] := AStyle;
end;

{ TCssStyleList }

procedure TCssStyleList.BuildDefaultStyles;

  procedure AddAlertStyle(AStyle: THtmlCssStyle; AFontColor, ABackgroundColor, ABorderColor: string);
  begin
    AStyle.Attribute[cssColor] := AFontColor;
    AStyle.Attribute[cssBackgroundColor] := ABackgroundColor;
    AStyle.Attribute[cssBorderColor] := ABorderColor;
  end;

var
  AStyle: THtmlCssStyle;
begin
  Style['body'].Attribute[cssTextAlign] := 'center';
  Style['body'].SetFontAttributes('Tahoma, Geneva, sans-serif', '#333333', '');

  Style['p'].Attribute[cssLineHeight] := '1.5';

  Style['p'].Attribute[cssFontFamily] := 'Tahoma, Geneva, sans-serif';

  AStyle := Style['.btn'];
  AStyle.Attribute[cssColor] := 'white';
  AStyle.Attribute[cssWidth] := '60%';
  AStyle.Attribute[cssMargin] := 'auto';
  AStyle.Attribute[cssBorder] := 'none';
  AStyle.SetFontAttributes('Tahoma, Geneva, sans-serif', '#fff', '');
  AStyle.Attribute[cssPadding] := '10px 20px';
  AStyle.Attribute[cssTextAlign] := 'center';
  AStyle.Attribute[cssTextDecoration] := 'none';
  AStyle.Attribute[cssCursor] := 'pointer';


  Style['.'+ButtonStyleToString(btnPrimary)].Attribute[cssBackground] := '#0d6efd';
  Style['.'+ButtonStyleToString(btnSecondary)].Attribute[cssBackground] := '#6C757D';
  Style['.'+ButtonStyleToString(btnSuccess)].Attribute[cssBackground] := '#198754';
  Style['.'+ButtonStyleToString(btnDanger)].Attribute[cssBackground] := '#DC3545';

  Style['.'+ButtonStyleToString(btnWarning)].Attribute[cssBackground] := '#FFC107';
  Style['.'+ButtonStyleToString(btnWarning)].Attribute[cssColor] := '#212529';

  Style['.'+ButtonStyleToString(btnInfo)].Attribute[cssBackground] := '#0DCAF0';
  Style['.'+ButtonStyleToString(btnInfo)].Attribute[cssColor] := '#fff';

  Style['.'+ButtonStyleToString(btnLight)].Attribute[cssBackground] := '#F8F9FA';
  Style['.'+ButtonStyleToString(btnLight)].Attribute[cssColor] := '#000';

  Style['.'+ButtonStyleToString(btnDark)].Attribute[cssBackground] := '#343A40';
  Style['.'+ButtonStyleToString(btnDark)].Attribute[cssColor] := '#000';

  Style['.'+ButtonStyleToString(btnLink)].Attribute[cssBackground] := 'none';
  Style['.'+ButtonStyleToString(btnLink)].Attribute[cssColor] := '#0d6efd';
  Style['.'+ButtonStyleToString(btnLink)].Attribute[cssTextDecoration] := 'underline';

  AStyle := Style['.alert'];
  AStyle.Attribute[cssPadding] := '10px';
  AStyle.Attribute[cssBorderWidth] := '1px';
  AStyle.Attribute[cssBorderStyle] := 'solid';
  AStyle.Attribute[cssBorderRadius] := '10px';
  AStyle.Attribute[cssMarginBottom] := '8px';

  AddAlertStyle(Style['.alert-success'], '#3c763d', '#dff0d8', '#BADFAA');
  AddAlertStyle(Style['.alert-danger'], '#B20000', '#FFB0B0', '#FF9999');
  AddAlertStyle(Style['.alert-warning'], '#B28500', '#FFFFBF', '#FFDC73');

end;


procedure TCssStyleList.GetHtml(AStrings: TStrings);
var
  AName: string;
  AStyle: THtmlCssStyle;
  ACssStrings: TStrings;
begin
  ACssStrings := TStringList.Create;
  try
    AStrings.Add('<style>');
    for AName in Self.Keys do
    begin
      if TryGetValue(AName, AStyle) then
      begin
        ACssStrings.Add(AName+' {'+AStyle.FAttributes.GetAsSingleLine+'}');
      end;
    end;
    TStringList(ACssStrings).Sort;
    AStrings.AddStrings(ACssStrings);
    AStrings.Add('.ExternalClass p, .ExternalClass span, .ExternalClass font, .ExternalClass td {line-height: 100%;}');
    AStrings.Add('.ExternalClass {width: 100%;}');



    AStrings.Add('</style>');
  finally
    ACssStrings.Free;
  end;
end;

function TCssStyleList.GetStyle(AName: string): THtmlCssStyle;
begin
  TryGetValue(AName.ToLower, Result);
  if Result = nil then
  begin
    Result := THtmlCssStyle.Create;
    AddOrSetValue(AName, Result);
  end;
end;

procedure TCssStyleList.LoadFromJson(AJson: TJsonArray);
var
  AObj: TJsonObject;
  ACss: THtmlCssStyle;
  {$IFNDEF USE_JSONDATAOBJECTS}
  AValue: TJSONValue;
  {$ENDIF}
  AName: string;
begin
  {$IFDEF USE_JSONDATAOBJECTS}
  for AObj in AJson do
  begin
    ACss := THtmlCssStyle.Create;
    AName := AObj.S['name'];    
    ACss.LoadFromJson(AObj);
    AddOrSetValue(AName, ACss);
  end;
  {$ELSE}
  for AValue in AJson do
  begin
    AObj := AValue as TJSONObject;
    ACss := THtmlCssStyle.Create;
    AName := AObj.Values['name'].Value;    
    ACss.LoadFromJson(AObj);
    AddOrSetValue(AName, ACss);
  end;  
  {$ENDIF}
end;

procedure TCssStyleList.SaveToJson(AJson: TJsonArray);
var
  AKey: string;
  AObj: TJsonObject;
  AStyle: THtmlCssStyle;
begin
  for AKey in Self.Keys do
  begin
    if TryGetValue(AKey, AStyle) then
    begin
      AObj := TJsonObject.Create;
      {$IFDEF USE_JSONDATAOBJECTS}
      AObj.S['name'] := AKey;
      {$ELSE}
      AObj.AddPair('name', AKey);
      {$ENDIF}
      AStyle.SaveToJson(AObj);
      AJson.Add(AObj);
    end;
  end;

end;

procedure TCssStyleList.SetAllHeaders(AAttribute: THtmlCssAttribute; AValue: string);
var
  h: THtmlHeaderType;
begin
  for h := Low(THtmlHeaderType) to High(THtmlHeaderType) do
  begin
    Style[TRttiEnumerationType.GetName(h)].Attribute[AAttribute] := AValue;
  end;
end;

{ TCssAttributeList }

function TCssAttributeList.GetAsSingleLine: string;
var
  AAttribute: THtmlCssAttribute;
  AValue: string;
begin
  Result := '';
  for AAttribute in Self.Keys do
  begin
    if TryGetValue(AAttribute, AValue) then
    begin
      Result := Result + CssAttributeNameToString(AAttribute)+': '+AValue+'; ';
    end;
  end;
end;


procedure BuildCssAttributeMap(AMap: THtmlCssAttributeMap);
var
  AAtt: THtmlCssAttribute;
begin
  AMap.Clear;
  AMap.Add(cssBackground, 'background');
  AMap.Add(cssBackgroundColor, 'background-color');
  AMap.Add(cssBorder, 'border');
  AMap.Add(cssBorderCollapse, 'border-collapse');
  AMap.Add(cssBorderColor, 'border-color');

  AMap.Add(cssBorderStyle, 'border-style');
  AMap.Add(cssBorderRadius, 'border-radius');
  AMap.Add(cssBorderWidth, 'border-width');
  AMap.Add(cssColor, 'color');
  AMap.Add(cssDisplay, 'display');
  AMap.Add(cssCursor, 'cursor');
  AMap.Add(cssFontFamily, 'font-family');
  AMap.Add(cssFontSize, 'font-size');
  AMap.Add(cssFontStyle, 'font-style');
  AMap.Add(cssFontWeight, 'font-weight');
  AMap.Add(cssHeight, 'height');
  AMap.Add(cssLineHeight, 'line-height');
  AMap.Add(cssMargin, 'margin');
  AMap.Add(cssMarginBottom, 'margin-bottom');
  AMap.Add(cssMarginTop, 'margin-top');
  AMap.Add(cssObjectFit, 'object-fit');
  AMap.Add(cssMaxHeight, 'max-height');
  AMap.Add(cssMaxWidth, 'max-width');
  AMap.Add(cssMinHeight, 'min-height');

  AMap.Add(cssPadding, 'padding');
  AMap.Add(cssTextAlign, 'text-align');
  AMap.Add(cssTextDecoration, 'text-decoration');
  AMap.Add(cssVerticalAlign, 'vertical-align');
  AMap.Add(cssWidth, 'width');
  AMap.Add(cssWhiteSpace, 'white-space');

  for AAtt := Low(THtmlCssAttribute) to High(THtmlCssAttribute) do
  begin
    if not AMap.ContainsKey(AAtt) then
      raise Exception.Create('THtmlCssAttribute map missing: '+ TRttiEnumerationType.GetName(AAtt));
  end;
end;

procedure TCssAttributeList.LoadFromJson(AJson: TJsonArray);
var
  {$IFDEF USE_JSONDATAOBJECTS}
  AStr: string;
  {$ELSE}
  AValue: TJSONValue;
  {$ENDIF}
  AStrings: TStrings;
  ICount: integer;
  
begin
  Clear;
  AStrings := TStringList.Create;
  try
    {$IFDEF USE_JSONDATAOBJECTS}
    for AStr in AJson do
      AStrings.Add(AStr);
    {$ELSE}
    for AValue in AJson do
      AStrings.Add(AValue.Value);
    
    {$ENDIF}
    for ICount := 0 to AStrings.Count-1 do
      AddOrSetValue(CssAttributeNameFromString(AStrings.Names[ICount]), AStrings.ValueFromIndex[ICount]);
  finally
    AStrings.Free;
  end;
end;

procedure TCssAttributeList.SaveToJson(AJson: TJsonArray);
var
  AKey: THtmlCssAttribute;
  AValue: string;
begin
  for AKey in Self.Keys do
  begin
    if TryGetValue(AKey, AValue) then
    begin
      AJson.Add(CssAttributeNameToString(AKey)+'='+AValue);
    end;
  end;
end;

{ THtmlParagraphElement }

function THtmlParagraphElement.GetTag(ATarget: THtmlRenderTarget): string;
begin
  Result := 'p';
end;

{ THtmlLinkElement }

function THtmlLinkElement.GetTag(ATarget: THtmlRenderTarget): string;
begin
  Result := 'a';
end;

{ THtmlAlertElement }

function THtmlAlertElement.GetTag(ATarget: THtmlRenderTarget): string;
begin
  Result := 'div';
end;

{ THtmlAttributeList }

procedure THtmlAttributeList.LoadFromJson(AArray: TJsonArray);
var
  {$IFDEF USE_JSONDATAOBJECTS}
  AStr: string;
  {$ELSE}
  AValue: TJSONValue;
  {$ENDIF}
  AStrings: TStrings;
  ICount: integer;
begin
  Clear;
  AStrings := TStringList.Create;
  try
    {$IFDEF USE_JSONDATAOBJECTS}
    for AStr in AArray do
      AStrings.Add(AStr);
    {$ELSE}
    for AValue in AArray do
      AStrings.Add(AValue.Value);
    {$ENDIF}
    for ICount := 0 to AStrings.Count-1 do
      AddOrSetValue(HtmlTagStringToAttribute(AStrings.Names[ICount]), AStrings.ValueFromIndex[ICount]);
  finally
    AStrings.Free;
  end;
end;

procedure THtmlAttributeList.SaveToJson(AArray: TJsonArray);
var
  AAtt: THtmlTagAttribute;
  AValue: string;
begin
  for AAtt in Self.Keys do
  begin
    if TryGetValue(AAtt, AValue) then
      AArray.Add(HtmlTagAttributeToString(AAtt)+'='+AValue);
  end;
end;

{ THmtlSocialIcons }

procedure THtmlSocialIcons.GetInternalHtml(AHtml: TStrings; ATarget: THtmlRenderTarget);

  function GetSvg(AName: string): string;
  var
    ResStream: TResourceStream;
    AStrings: TStrings;
  begin
    ResStream := TResourceStream.Create(hInstance, AName, RT_RCDATA);
    AStrings := TStringList.Create;
    try
      ResStream.Position := 0;
      AStrings.LoadFromStream(ResStream);
      Result := Trim(AStrings.Text);
    finally
      AStrings.Free;
      ResStream.Free;
    end;
  end;

begin
  inherited;
  if FSocialUrls.FTwitterUrl <> '' then AHTml.Add('<a href="'+FSocialUrls.FTwitterUrl+'"><img border="0" width="24" height="24" src="'+C_TWITTER_ICON_URL+'"/></a>');
  if FSocialUrls.FInstagramUrl <> '' then AHTml.Add('<a href="'+FSocialUrls.FInstagramUrl+'"><img border="0" width="24" height="24" src="'+C_INSTAGRAM_ICON_URL+'"/></a>');
  if FSocialUrls.FFacebookUrl <> '' then AHTml.Add('<a href="'+FSocialUrls.FFacebookUrl+'"><img border="0" width="24" height="24" src="'+C_FACEBOOK_ICON_URL+'"/></a>');
end;

{ THtmlRawElement }

function THtmlRawElement.GetTag(ATarget: THtmlRenderTarget): string;
begin
  Result := '';
end;

{ THtmlButtonElement }

function THtmlButtonElement.GetTag(ATarget: THtmlRenderTarget): string;
begin
  Result := 'button';
end;

initialization

  InternalHtmlCssAttributeMap := THtmlCssAttributeMap.Create;
  BuildCssAttributeMap(InternalHtmlCssAttributeMap);

  RegisterClass(THtmlDivElement);
  RegisterClass(THtmlRawElement);
  RegisterClass(THtmlHeaderElement);
  RegisterClass(THtmlHrElement);
  RegisterClass(THtmlBrElement);
  RegisterClass(THtmlImageElement);
  RegisterClass(THtmlParagraphElement);
  RegisterClass(THtmlAlertElement);
  RegisterClass(THtmlButtonElement);
  RegisterClass(THtmlLinkElement);

finalization

  InternalHtmlCssAttributeMap.Free;

end.
