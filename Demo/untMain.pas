unit untMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.OleCtrls, SHDocVw, Vcl.ExtCtrls, ksHtmlBuilder;

type
  TForm22 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    ListBox1: TListBox;
    Panel3: TPanel;
    rbHtmlEmail: TRadioButton;
    rbHtmlCss: TRadioButton;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Memo1: TMemo;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    WebBrowser1: TWebBrowser;
    procedure ListBox1Click(Sender: TObject);
    procedure rbHtmlCssClick(Sender: TObject);
    procedure rbHtmlEmailClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private


    procedure PopulateHtml;
    procedure PaymentFailedTemplate(AHtml: IHtmlDocument);
    procedure CancelBookingTemplate(AHtml: IHtmlDocument);
    procedure VoucherTemplate(AHtml: IHtmlDocument);
    procedure PopulateBootstrapAlerts(AHtml: IHtmlDocument);
    procedure PopulateBootstrapButtons(AHtml: IHtmlDocument);
    procedure PopulateParagraphAlignment(AHtml: IHtmlDocument);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form22: TForm22;

implementation

const
  C_HEADER_IMG = 'https://www.publicdomainpictures.net/pictures/150000/velka/banner-header-tapete-1450368431G3V.jpg';

{$R *.dfm}

function CreateEmailTemplateNew: IHtmlDocument;
var
  AStyles: TCssStyleList;
  p: THtmlParagraphElement;
  AImg: THtmlImageElement;
begin
  Result := CreateHtmlDocument;

  AStyles := Result.Head.Styles;
  AStyles.Style['body'].SetFontAttributes('Verdana, sans-serif', '#333333', '14px');
  AStyles.SetAllHeaders(cssFontFamily, 'Verdana, sans-serif');
  AStyles.SetAllHeaders(cssColor, '#005CB7;');
  AStyles.SetAllHeaders(cssFontWeight, '400');

  //p := Result.Footer.Elements.AddParagraph('powered by');
  //p.Style[cssColor] := '#666666';
  //p.Style[cssMargin] := '0px';
  //AImg := Result.Footer.Elements.AddImageFromUrl('https://upload.wikimedia.org/wikipedia/commons/b/bd/Delphi_Language_Logo.png');
 // AImg.Style[cssWidth] := '36px';
end;


procedure TForm22.PopulateBootstrapButtons(AHtml: IHtmlDocument);
var
  e: THtmlElementList;
begin
  e := AHtml.Elements;
  e.AddHeader(h2, 'Bootstrap Style Buttons');
  e.AddHr;
  e.AddParagraph('(These are defined using pure css so do not require bootstrap)');
  e.AddButton('Primary Button', '', THtmlButtonStyle.btnPrimary);
  e.AddButton('Secondary Button', '', THtmlButtonStyle.btnSecondary);
  e.AddButton('Success Button', '', THtmlButtonStyle.btnSuccess);
  e.AddButton('Danger Button', '', THtmlButtonStyle.btnDanger);

  e.AddButton('Warning Button', '', THtmlButtonStyle.btnWarning);
  e.AddButton('Info Button', '', THtmlButtonStyle.btnInfo);
  e.AddButton('Light Button', '', THtmlButtonStyle.btnLight);
  e.AddButton('Link Button', '', THtmlButtonStyle.btnLink);
end;

procedure TForm22.PopulateBootstrapAlerts(AHtml: IHtmlDocument);
var
  e: THtmlElementList;
begin
  e := AHtml.Elements;

  e.AddHeader(h2, 'Bootstrap Style Alerts');
  e.AddHr;
  e.AddParagraph('(These are defined using pure css so do not require bootstrap)');
  e.AddSpacer(20);
  e.AddAlert('Success alert example', THtmlAlertStyle.asSuccess);
  e.AddAlert('Danger alert example', THtmlAlertStyle.asDanger);
  e.AddAlert('Warning alert example', THtmlAlertStyle.asWarning);

end;

procedure TForm22.PopulateParagraphAlignment(AHtml: IHtmlDocument);
var
  e: THtmlElementList;
  p: THtmlParagraphElement;
begin
  e := AHtml.Elements;

  e.AddHeader(h2, 'Paragraphs');
  e.AddHr;
  p := e.AddParagraph('Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industrys '+
                 'standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a '+
                 'type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, '+
                 'remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing '+
                 'Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions '+
                 'of Lorem Ipsum.');
  p.Style[cssTextAlign] := 'justify';
  p.Style[cssTextAlignlast] := 'center';

end;


procedure TForm22.PaymentFailedTemplate(AHtml: IHtmlDocument);
var
  e: THtmlElementList;
begin
  //AHtml.HeaderBanner.Src := 'https://kernow-s3.s3.eu-west-1.amazonaws.com/www/online-pay-banner.jpg';

  e := AHtml.Elements;

  e.AddHeader(h2, 'Payment Failed');
  e.AddHr;
  e.AddSpacer(20);

  e.AddParagraph('We were unable to collect the latest subscription payment.');
  e.AddParagraph('Please use the button below to update your payment method.');
  e.AddHr;

  e.AddSpacer(20);
  e.AddImageFromUrl('https://kernow-s3.s3.eu-west-1.amazonaws.com/www/uk-direct-debit-logo.png').Style[cssWidth] := '120px';
  e.AddHeader(h3, 'New Payment Option');
  e.AddParagraph('We now support Direct Debit (BACS) payments which you can use instead of adding a credit/debit card.');
  e.AddSpacer(20);


  e.AddButton('Update Payment Method', '#', btnPrimary);
  e.AddSpacer(20);
  e.AddParagraph('If you have any questions please get in touch.');


end;

procedure TForm22.PopulateHtml;
var
  Doc: Variant;
  s: string;
  AHtml: IHtmlDocument;
begin
  if not Assigned(WebBrowser1.Document) then
    WebBrowser1.Navigate('about:blank');

  if ListBox1.ItemIndex = -1 then Exit;
  s := ListBox1.Items[ListBox1.ItemIndex].ToLower;

  AHtml := CreateEmailTemplateNew;
  if s = 'bootstrap style alerts' then PopulateBootstrapAlerts(AHtml);
  if s = 'bootstrap style buttons' then PopulateBootstrapButtons(AHtml);
  if s = 'payment failed' then PaymentFailedTemplate(AHtml);
  if s = 'cancel booking' then CancelBookingTemplate(AHtml);
  if s = 'voucher' then VoucherTemplate(AHtml);
  if s = 'text justify' then PopulateParagraphAlignment(AHtml);

  if rbHtmlCss.Checked then memo1.Text := AHtml.AsHtml;
  if rbHtmlEmail.Checked then memo1.Text := AHtml.AsHtml;

  Doc := WebBrowser1.Document;
  Doc.Clear;
  Doc.Write(memo1.Text);

  Doc.Close;

  WebBrowser1.Refresh;
end;

procedure TForm22.VoucherTemplate(AHtml: IHtmlDocument);
var
  e: THtmlElementList;
begin
  //AHtml.HeaderBanner.Src := 'https://kernow-s3.s3.eu-west-1.amazonaws.com/www/gift-voucher.jpg';
  e := AHtml.Elements;
  e.AddHeader(h2, 'Gift Voucher');
  e.AddHeader(h3, 'The Demo Bistro (£50)');
  e.AddSpacer(10);
  e.AddParagraph('Dear John,');
  e.AddSpacer(10);
  e.AddParagraph('Your voucher is attached to this email.');
  e.AddSpacer(10);
  e.AddParagraph('Kind regards,');
  e.AddParagraph('<b>The Demo Bistro</b>');
  e.AddSpacer(10);
  e.AddHr;
  e.AddSpacer(10);
  e.AddParagraph('The Demo Bistro, London, SW1 123').SetFontAttributes('', '#999', '10px');
end;

procedure TForm22.FormCreate(Sender: TObject);
begin
  ListBox1.ItemIndex := 0;
  PopulateHtml;
end;

procedure TForm22.FormResize(Sender: TObject);
begin
  Panel8.Width := Width div 2;
end;

procedure TForm22.ListBox1Click(Sender: TObject);
begin
  PopulateHtml;
end;

procedure TForm22.CancelBookingTemplate(AHtml: IHtmlDocument);
var
  e: THtmlElementList;
  ASocialUrls: THtmlSocialUrls;
begin
  e := AHtml.Elements;
  e.AddHeader(h2, 'Reservation Cancelled');

  e.AddHr;
  e.AddParagraph('We have now cancelled the following reservation.');
  e.AddHr;
  e.AddParagraph('John Smith'+'<br>'+
                 FormatDateTime('ddd, d mmm, yy', Now)+' at '+FormatDateTime('h:nn am/pm', Now)+'<br>'+
                 'Dinner'+'<br>'+
                 'x '+2.ToString+' guests');
  e.AddSpacer(10);
  e.AddHr;
  e.AddParagraph('Thank you for letting us know.');

  ASocialUrls.FFacebookUrl := 'www.facebook.com';
  ASocialUrls.FInstagramUrl := 'www.instagram.com';
  ASocialUrls.FTwitterUrl := 'https://www.twitter.com';
  e.AddSocialIcons(ASocialUrls);
end;

procedure TForm22.rbHtmlCssClick(Sender: TObject);
begin
  PopulateHtml;
end;

procedure TForm22.rbHtmlEmailClick(Sender: TObject);
begin
  PopulateHtml;
end;

end.
