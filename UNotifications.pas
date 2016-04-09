unit UNotifications;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TEvent = procedure() of object;
  TNotificationEvent = procedure(Sender: TObject) of object;
  TNotificationClass = array of integer;   //< Class???   //<= Vector
  TSubscriber = class;

  TSubscriber = class
  private
    FRecieveAll: boolean;
    FRecieveEnable: boolean;
    FSendEnable: boolean;
    FNotificationClass: TNotificationClass;
    FParent: TSubscriber;
    FSubscribers: array of TSubscriber;     //<= Vector
    FOnNotificationRecieve: TNotificationEvent;
    function Containing(ANotificationClass: TNotificationClass): boolean;
    procedure NotificationRecieve(Sender: TObject;
      ANotificationClass: TNotificationClass);
    procedure NotifySubscribers(Sender: TObject; ANotificationClass: TNotificationClass);
  public
    constructor Create(RecieveAll: boolean = False);
    destructor Destroy;
    procedure Subscribe(Subscriber: TSubscriber);
    procedure UnSubscribe(Subscriber: TSubscriber);
    procedure CreateNotification(Sender: TObject;
      ANotificationClass: TNotificationClass);
  published
    property OnNotificationRecieve: TNotificationEvent write FOnNotificationRecieve;
    property Parent: TSubscriber read FParent write FParent;
    property NotificationClass: TNotificationClass
      read FNotificationClass write FNotificationClass;
  end;

function ToNotificationClass(A: array of integer): TNotificationClass;

implementation

function ToNotificationClass(A: array of integer): TNotificationClass;
var
  i: integer;
begin
  SetLength(Result, Length(A));
  for i := 0 to High(A) do
    Result[i] := A[i];
end;

constructor TSubscriber.Create(RecieveAll: boolean = False);
begin
  FParent := nil;
  SetLength(FSubscribers, 0);
  FRecieveAll := RecieveAll;
  FSendEnable := True;
  FRecieveEnable := True;
end;

destructor TSubscriber.Destroy;
var
  i: integer;
begin
  if FParent <> nil then
    FParent.UnSubscribe(Self);
  for i := 0 to High(FSubscribers) do
    FSubscribers[i].Destroy;
end;

function TSubscriber.Containing(ANotificationClass: TNotificationClass): boolean;
var
  i: integer;
  j: integer;
begin
  if FRecieveAll then
    Exit(True);
  if not FRecieveEnable then
    Exit(False);
  for i := 0 to Length(FNotificationClass) - 1 do
    for j := 0 to Length(ANotificationClass) - 1 do
      if (FNotificationClass[i] = ANotificationClass[j]) then
        Exit(True);
  Exit(False);
end;

procedure TSubscriber.Subscribe(Subscriber: TSubscriber);
begin
  SetLength(FSubscribers, Length(FSubscribers) + 1);
  FSubscribers[High(FSubscribers)] := Subscriber;
  Subscriber.Parent := Self;
end;

procedure TSubscriber.UnSubscribe(Subscriber: TSubscriber);
var
  i: integer;
  Temp: TSubscriber;
begin
  for i := 0 to High(FSubscribers) do
    if Subscriber = FSubscribers[i] then begin
      Temp := FSubscribers[High(FSubscribers)];
      FSubscribers[High(FSubscribers)] := FSubscribers[i];
      FSubscribers[i] := Temp;
      FSubscribers[High(FSubscribers)].Free;
      SetLength(FSubscribers, Length(FSubscribers) - 1);
      Exit;
    end;
end;

procedure TSubscriber.CreateNotification(Sender: TObject;
  ANotificationClass: TNotificationClass);
var
  Iterator: TSubscriber;
begin
  Iterator := Self;
  while (Iterator.FParent <> nil) and (Iterator.FParent.Containing(NotificationClass)) do
    Iterator := Iterator.FParent;
  Iterator.NotifySubscribers(Sender, ANotificationClass);
end;

procedure TSubscriber.NotificationRecieve(Sender: TObject;
  ANotificationClass: TNotificationClass);
begin
  if not FRecieveEnable then
    Exit;
  if FOnNotificationRecieve <> nil  then
    FOnNotificationRecieve(Sender);
  NotifySubscribers(Sender, ANotificationClass);
end;

procedure TSubscriber.NotifySubscribers(Sender: TObject;
  ANotificationClass: TNotificationClass);
var
  i: integer;
begin
  for i := 0 to High(FSubscribers) do
    if FSubscribers[i].Containing(ANotificationClass) then
      FSubscribers[i].NotificationRecieve(Sender, ANotificationClass);
end;

end.
