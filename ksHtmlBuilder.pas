unit ksHtmlBuilder;

interface

uses Classes, System.Generics.Collections, Graphics, JsonDataObjects;

type
  IHtmlDocumentNew = interface;

  THtmlCssStyle = class;
  THtmlElement = class;
  THtmlDivElement = class;
  THtmlHeaderElement = class;
  THtmlHrElement = class;
  THtmlBrElement = class;
  THtmlImageElement = class;
  THtmlParagraphElement = class;
  THtmlAlertElement = class;
  THtmlButtonElement = class;

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
    cssMaxWidth,
    cssMinHeight,cssObjectFit,
    cssPadding,
    cssTextAlign,
    cssTextDecoration,
    cssVerticalAlign,
    cssWidth,
    cssWhiteSpace
  );

  THtmlTagAttribute = (attCellSpacing, attCellPadding, attHeight, attHref, attSrc, attWidth);

  THtmlButtonStyle = (btnPrimary, btnSecondary, btnSuccess, btnDanger, btnWarning, btnInfo, btnLight, btnLink);
  THtmlAlertStyle = (asSuccess, asDanger, asWarning);


  TCssAttributeList = class(TDictionary<THtmlCssAttribute, string>)
  private
    function GetAsSingleLine: string;
  public
    procedure SaveToJson(AJson: TJsonArray);
    property AsSingleLine: string read GetAsSingleLine;
  end;

  THtmlAttributeList = class(TDictionary<THtmlTagAttribute, string>)
  private
  end;

  TCssStyleList = class(TObjectDictionary<string, THtmlCssStyle>)
  private
    function GetStyle(AName: string): THtmlCssStyle;
    procedure BuildDefaultStyles;
  public
    procedure SaveToJson(AJson: TJsonArray);
    procedure GetHtml(AStrings: TStrings);
    procedure SetAllHeaders(AAttribute: THtmlCssAttribute; AValue: string);
    property Style[AName: string]: THtmlCssStyle read GetStyle; default;
  end;


  THtmlElementList = class(TObjectList<THTmlElement>)
  private
    [weak] FDocument: IHtmlDocumentNew;
    FOwner: THTmlElement;
    function AddImageStream(AStream: TStream): THtmlImageElement;
  public
    constructor Create(AOwner: THTmlElement; ADocument: IHtmlDocumentNew);
    function AddButton(AText, AUrl: string; AStyle: THtmlButtonStyle): THtmlButtonElement;
    function AddDiv: THtmlDivElement;
    function AddSpacer(AHeight: integer): THtmlDivElement;
    function AddHeader(AType: THtmlHeaderType; AText: string): THtmlHeaderElement;
    function AddHr: THtmlHrElement;
    function AddBr: THtmlBrElement;

    function AddImage(AImg: TGraphic): THtmlImageElement; overload;
    function AddImageFromFile(AFilename: string): THtmlImageElement; overload;
    function AddImageFromUrl(ASrc: string; const AInline: Boolean = False): THtmlImageElement; overload;
    function AddParagraph(AText: string): THtmlParagraphElement;
    function AddAlert(AText: string; AAlertStyle: THtmlAlertStyle): THtmlAlertElement;
  end;

  THtmlCssStyle = class
  private
    //FName: string;
    FAttributes: TCssAttributeList;
    function GetAttribute(AName: THtmlCssAttribute): string;
    procedure SetAttribute(AName: THtmlCssAttribute; const Value: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SaveToJson(AJson: TJsonObject);
    procedure GetHtml(AStrings: TStrings);
    procedure SetBorderAttributes(AColor, AWidth, AStyle: string);
    procedure SetFontAttributes(AFamily, AColor, ASize: string);
    property Attribute[AName: THtmlCssAttribute]: string read GetAttribute write SetAttribute;
  end;

  THtmlElement = class
  private
    [weak] FDocument: IHtmlDocumentNew;
    FParent: THTmlElement;
    FElements: THtmlElementList;
    FContent: string;
    FClass: TStrings;
    FAttributes: THtmlAttributeList;
    FStyles: TCssAttributeList;

    function GetStyle(AStyle: THtmlCssAttribute): string;
    function GetAttributesSingleLine(ATarget: THtmlRenderTarget): string;
    function GetStylesSingleLine: string;
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
    constructor Create(ADocument: IHtmlDocumentNew; AParent: THtmlElement); virtual;
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

  THtmlButtonElement = class(THtmlLinkElement);

  THtmlHeadSection = class(THtmlElement)
  private
    FCssStyles: TCssStyleList;
  protected
    function GetTag(ATarget: THtmlRenderTarget): string; override;
    procedure GetInternalHtml(AStrings: TStrings; ATarget: THtmlRenderTarget); override;
  public
    constructor Create(ADocument: IHtmlDocumentNew; AParent: THtmlElement); override;
    destructor Destroy; override;
    procedure SaveToJson(AJson: TJsonObject); override;

    property Styles: TCssStyleList read FCssStyles;
  end;

  THtmlRoot = class(THtmlElement)
  protected
    function GetTag(ATarget: THtmlRenderTarget): string; override;
    procedure GetHtml(AHtml: TStrings; ATarget: THtmlRenderTarget); override;
  end;



  THtmlBodySection = class(THtmlElement)
  protected
    function GetTag(ATarget: THtmlRenderTarget): string; override;
  end;

  IHtmlDocumentNew = interface
    ['{6B7716EE-3A39-493F-89D4-60077631259E}']
    function GetHead: THtmlHeadSection;
    function GetHeaderBanner: THtmlImageElement;
    function GetContainer: THtmlDivElement;
    function GetFooter: THtmlDivElement;
    function GetAsHtml(ATarget: THtmlRenderTarget): string;

    procedure SaveToJson(AJson: TJsonObject);

    procedure SaveHtmlToFile(AFilename: string; const AFormat: THtmlRenderTarget = htmlBrowser);
    property Head: THtmlHeadSection read GetHead;
    property HeaderBanner: THtmlImageElement read GetHeaderBanner;
    property Container: THtmlDivElement read GetContainer;
    property Footer: THtmlDivElement read GetFooter;
    property AsHtml[ATarget: THtmlRenderTarget]: string read GetAsHtml;
  end;


  function CreateHtmlDocument: IHtmlDocumentNew;

implementation

uses SysUtils, Rtti, Net.HttpClient, System.NetEncoding, Jpeg;

type
  THtmlCssAttributeMap = TDictionary<THtmlCssAttribute,string>;

  THtmlDocument = class(TInterfacedObject, IHtmlDocumentNew)
  private
    FRoot: THtmlRoot;
    FHead: THtmlHeadSection;
    FHeaderBanner: THtmlImageElement;

    FBody: THtmlBodySection;
    FEmailFormat: Boolean;
    FContainer: THtmlDivElement;
    FFooter: THtmlDivElement;
    function GetAsHtml(ATarget: THtmlRenderTarget): string;
    function GetContainer: THtmlDivElement;
    function GetHead: THtmlHeadSection;
    function GetFooter: THtmlDivElement;
    function GetHeaderBanner: THtmlImageElement;
  protected
    procedure SaveHtmlToFile(AFilename: string; const AFormat: THtmlRenderTarget = htmlBrowser);
    procedure SaveToJson(AJson: TJsonObject);

    property Head: THtmlHeadSection read GetHead;
    property HeaderBanner: THtmlImageElement read GetHeaderBanner;
    property Container: THtmlDivElement read GetContainer;
    property Footer: THtmlDivElement read GetFooter;
    property AsHtml[ATarget: THtmlRenderTarget]: string read GetAsHtml;
   //property AsEmailHtml: string read GetAsEmailHtml;

  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;


var
  InternalHtmlCssAttributeMap: THtmlCssAttributeMap;


function CreateHtmlDocument: IHtmlDocumentNew;
begin
  Result := THtmlDocument.Create;
end;

{ THtmlDivElement }

function HtmlSectionToString(AElement: THtmlElement): string;
begin
  Result := TRttiEnumerationType.GetName(AElement);
  Result := StringReplace(Result, 'html', '', []).ToLower;
end;

function HtmlTagAttributeToString(AAttribute: THtmlTagAttribute): string;
begin
  Result := LowerCase(StringReplace(TRttiEnumerationType.GetName(AAttribute), 'att', '', [rfReplaceAll]));

end;

function CssAttributeNameToString(AStyle: THtmlCssAttribute): string;
begin
  InternalHtmlCssAttributeMap.TryGetValue(AStyle, Result);
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
  //if ATarget = htmlEmail then
  //  Result := 'table';
end;

{ THtmlElement }

constructor THtmlElement.Create(ADocument: IHtmlDocumentNew; AParent: THtmlElement);
begin
  FElements := THtmlElementList.Create(Self, ADocument);
  FClass := TStringList.Create;
  FAttributes := THtmlAttributeList.Create;
  FStyles := TCssAttributeList.Create;
  FDocument := ADocument;
  FParent := AParent;
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


  ABlock := '<'+_ATag;


  ABlock := Trim(ABlock + ' '+GetClassSingleLine);
  ABlock := Trim(ABlock + ' '+GetAttributesSingleLine(ATarget));
  ABlock := Trim(ABlock + ' '+GetStylesSingleLine);
  ABlock := ABlock + '>';

  if _ATag = 'table' then
  begin
    ACellCss := '';
    APadding := Style[cssPadding];
    if APadding <> '' then ACellCss := 'style="padding:'+APadding+';"';
    ABlock := ABlock+'<tr><td '+ACellCss+'>';
  end;
  AHtml.Add(ABlock);


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
  ACssAttribute: THtmlCssAttribute;
  AStyle: THtmlCssStyle;
  AValue: string;
  AStrings: TStrings;
  AStr: string;
  ICount: integer;
begin
  Result := '';
  if ATarget = htmlEmail then
  begin
    // populate inline styles from css definitions...
    AStrings := TStringList.Create;
    try

      AStrings.Add(GetTag(ATarget));

      for ICount := 0 to FClass.Count-1 do
        AStrings.Add('.'+FClass[ICount]);



    

      for AStr in AStrings do
      begin
        AStyle := FDocument.Head.Styles.GetStyle(AStr);
        for ACssAttribute in AStyle.FAttributes.Keys do
        begin
          AStyle.FAttributes.TryGetValue(ACssAttribute, AValue);
          if AValue <> '' then
          begin
            if Style[ACssAttribute] = '' then
            begin
              Style[ACssAttribute] := AValue;
            end;
          end;
        end;
      end;
    finally
      AStrings.Free;
    end;
  end;


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

function THtmlElement.GetStylesSingleLine: string;
var
  AStyle: THtmlCssAttribute;
begin
  Result := '';
  if FStyles.Count > 0 then
  begin
    
    Result := Result + ' style="';
    for AStyle in FStyles.Keys do
    begin
      if FStyles[AStyle] <> '' then
      begin
        Result := Trim(Result + ' '+CssAttributeNameToString(AStyle)+': '+FStyles.Items[AStyle]+';');
      end;
    end;
    Result := Result + '" ';
  end;
end;

function THtmlElement.HasClosingTag: Boolean;
begin
  Result := True;
end;

procedure THtmlElement.SaveToJson(AJson: TJsonObject);
var
  AElement: THtmlElement;
  AObj: TJsonObject;
begin
  AJson.S['obj'] :=  GetTag(htmlBrowser);
  if FClass.Count > 0 then AJson.S['class'] := FClass.CommaText;
  if FContent <> '' then AJson.S['content'] := FContent;
  for AElement in FElements do
  begin
    AObj := TJsonObject.Create;
    AElement.SaveToJson(AObj);
    AJson.A['objects'].Add(AObj);
  end;
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

constructor THtmlHeadSection.Create(ADocument: IHtmlDocumentNew; AParent: THtmlElement);
begin
  inherited;
  FCssStyles := TCssStyleList.Create([doOwnsValues]);
  FCssStyles.BuildDefaultStyles;
end;

destructor THtmlHeadSection.Destroy;
begin
  FCssStyles.Free;
  inherited;
end;

procedure THtmlHeadSection.GetInternalHtml(AStrings: TStrings; ATarget: THtmlRenderTarget);
begin
  inherited;
  AStrings.Add('<meta http-equiv="Content-Type" content="text/html charset=UTF-8" />');
  AStrings.Add('<meta http-equiv="X-UA-Compatible" content="IE=edge">');
  AStrings.Add('<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0">');
  if ATarget <> htmlEmail then
  begin
    FCssStyles.GetHtml(AStrings);
  end;
end;

function THtmlHeadSection.GetTag(ATarget: THtmlRenderTarget): string;
begin
  Result := 'head';
end;

procedure THtmlHeadSection.SaveToJson(AJson: TJsonObject);
begin
  inherited;
  FCssStyles.SaveToJson(AJson.A['styles']);
end;

{ THtmlDocument }

constructor THtmlDocument.Create;
var
  AOuterContainer: THtmlDivElement;
begin

  inherited Create;
  FRoot := THtmlRoot.Create(Self, nil);

  FHead := THtmlHeadSection.Create(Self, nil);
  FRoot.Elements.Add(FHead);

  FBody := THtmlBodySection.Create(Self, nil);;
  FRoot.Elements.Add(FBody);


  FEmailFormat := False;
  fBody.Style[cssBackgroundColor] := '#eeeeee';
  fBody.Style[cssPadding] := '24px';

  AOuterContainer := FBody.Elements.AddDiv;
  AOuterContainer.FClass.Add('outerContainer');
  AOuterContainer.Style[cssMargin] := '0 auto';
  AOuterContainer.SetBorderAttributes('#a6a6a6', '1px', 'solid');
  AOuterContainer.Style[cssBackgroundColor] := 'white';
  AOuterContainer.Style[cssMaxWidth] := '600px';
  AOuterContainer.Style[cssMinHeight] := '200px';
  AOuterContainer.Style[cssTextAlign] := 'center';

  FHeaderBanner := AOuterContainer.Elements.AddImageFromUrl('');
  FHeaderBanner.Style[cssWidth] := '100%';

  FContainer := AOuterContainer.Elements.AddDiv;


  FContainer.Style[cssPadding] := '24px';
  FBody.Elements.AddSpacer(20);
  FFooter := FBody.Elements.AddDiv;
end;

destructor THtmlDocument.Destroy;
begin
  FRoot.Free;
  //FHead.Free;
  //FBody.Free;
  inherited;
end;
      {
function THtmlDocument.GetAsEmailHtml: string;
begin
  FEmailFormat := True;
  try
    Result := GetAsHtml;
  finally
    FEmailFormat := False;
  end;
end;     }

function THtmlDocument.GetAsHtml(ATarget: THtmlRenderTarget): string;
var
  AStrings: TStrings;
begin
  AStrings := TStringList.Create;
  try
    FRoot.GetHtml(AStrings, ATarget);
    {AStrings.Add('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">');
    AStrings.Add('<html>');
    FHead.GetHtml(AStrings, ATarget);
    FBody.GetHtml(AStrings, ATarget);
    AStrings.Add('</html>');}
    Result := AStrings.Text;
  finally
    AStrings.Free;
  end;
end;

function THtmlDocument.GetContainer: THtmlDivElement;
begin
  Result := FContainer;
end;

function THtmlDocument.GetFooter: THtmlDivElement;
begin
  Result := FFooter;
end;

function THtmlDocument.GetHead: THtmlHeadSection;
begin
  Result := FHead;
end;

function THtmlDocument.GetHeaderBanner: THtmlImageElement;
begin
  Result := FHeaderBanner;
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
var
  AObj: TJSONObject;

begin
  AObj := TJSONObject.Create;
  try
    FRoot.SaveToJson(AJson);
  finally
    AObj.Free;
  end;
end;

{ THtmlBodySection }

function THtmlBodySection.GetTag(ATarget: THtmlRenderTarget): string;
begin
  Result := 'body';
end;

{ THtmlElementList }

function THtmlElementList.AddButton(AText, AUrl: string; AStyle: THtmlButtonStyle): THtmlButtonElement;
begin
  Result := THtmlButtonElement.Create(FDocument, FOwner);
  Result.Attribute[attHref] := AUrl;
  Result.CssClass.Add('btn');
  Result.CssClass.Add(ButtonStyleToString(AStyle));
  Result.Text := AText;
  Add(Result);
end;

function THtmlElementList.AddDiv: THtmlDivElement;
begin
  Result := THtmlDivElement.Create(FDocument, FOwner);
  Add(Result);
end;

function THtmlElementList.AddHeader(AType: THtmlHeaderType; AText: string): THtmlHeaderElement;
begin
  Result := THtmlHeaderElement.Create(FDocument, FOwner);
  Result.HeaderType := AType;
  Result.Text := AText;
  Add(Result);
end;

function THtmlElementList.AddHr: THtmlHrElement;
begin
  Result := THtmlHrElement.Create(FDocument, FOwner);
  Result.Style[cssBorder] := 'none';
  Result.Style[cssHeight] := '1px';
  Result.Style[cssBackgroundColor] := '#ccc';
  Add(Result);
end;

function THtmlElementList.AddAlert(AText: string; AAlertStyle: THtmlAlertStyle): THtmlAlertElement;
begin
  Result := THtmlAlertElement.Create(FDocument, FOwner);
  Result.Style := AAlertStyle;
  Result.Text := AText;
  Result.FClass.Add('alert');
  Result.FClass.Add(AlertStyleToString(AAlertStyle));
  Add(Result);
end;

function THtmlElementList.AddBr: THtmlBrElement;
begin
  Result := THtmlBrElement.Create(FDocument, FOwner);
  Add(Result);
end;

function THtmlElementList.AddImageStream(AStream: TStream): THtmlImageElement;
var
  AEncoded: TStringStream;
begin
  Result := THtmlImageElement.Create(FDocument, FOwner);
  AEncoded := TStringStream.Create;
  try
    TNetEncoding.Base64.Encode(AStream, AEncoded);
    Result.Attribute[attSrc] := 'data:image/png;base64, '+AEncoded.DataString;
  finally
    AEncoded.Free;
  end;
  Add(Result);

end;

function THtmlElementList.AddParagraph(AText: string): THtmlParagraphElement;
begin
  Result := THtmlParagraphElement.Create(FDocument, FOwner);
  Result.Text := AText;
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
    Result := THtmlImageElement.Create(FDocument, FOwner);
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

constructor THtmlElementList.Create(AOwner: THTmlElement; ADocument: IHtmlDocumentNew);
begin
  inherited Create(True);
  FOwner := AOwner;
  FDocument := ADocument;
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

procedure THtmlCssStyle.SaveToJson(AJson: TJsonObject);
begin
  FAttributes.SaveToJson(AJson.A['attributes'])
end;

procedure THtmlCssStyle.SetAttribute(AName: THtmlCssAttribute; const Value: string);
begin
  FAttributes.AddOrSetValue(AName, Value);
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
  Style['p'].Attribute[cssLineHeight] := '1.6';

  AStyle := Style['.btn'];
  AStyle.Attribute[cssColor] := 'white';
  AStyle.Attribute[cssBorder] := 'none';
  AStyle.SetFontAttributes('Arial, Helvetica, sans-serif', '#fff', '16px');
  AStyle.Attribute[cssMargin] := 'auto';
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
  Style['.'+ButtonStyleToString(btnInfo)].Attribute[cssColor] := '#212529';

  Style['.'+ButtonStyleToString(btnLight)].Attribute[cssBackground] := '#F8F9FA';
  Style['.'+ButtonStyleToString(btnLight)].Attribute[cssColor] := '#212529';

  Style['.'+ButtonStyleToString(btnDanger)].Attribute[cssBackground] := '#212529';
  Style['.'+ButtonStyleToString(btnLink)].Attribute[cssBackground] := 'none';
  Style['.'+ButtonStyleToString(btnLink)].Attribute[cssColor] := '#0d6efd';
  Style['.'+ButtonStyleToString(btnLink)].Attribute[cssTextDecoration] := 'underline';

  AStyle := Style['.alert'];
  AStyle.Attribute[cssFontSize] := '10pt';
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
    //Result.FName := AName;
    AddOrSetValue(AName, Result);
  end;
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
      AObj.S['name'] := AKey;
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
  AMap.Add(cssMarginTop, 'margin-top8');
  AMap.Add(cssObjectFit, 'object-fit');
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

{ THtmlRoot }

procedure THtmlRoot.GetHtml(AHtml: TStrings; ATarget: THtmlRenderTarget);
begin
  inherited;
  AHtml.Add('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">');
end;

function THtmlRoot.GetTag(ATarget: THtmlRenderTarget): string;
begin
  Result := 'html';
end;

initialization

  InternalHtmlCssAttributeMap := THtmlCssAttributeMap.Create;
  BuildCssAttributeMap(InternalHtmlCssAttributeMap);

finalization

  InternalHtmlCssAttributeMap.Free;

end.
