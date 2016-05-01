unit UNotifications;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, UVector;

type

  TEvent = procedure of object;
  TNotificationEvent = procedure(Sender: TObject) of object;
  //Notification class
  TNClass = array of integer;
  TSubscriber = class;

  TSubscriber = class
  private
    type
    TSubsribers = specialize TObjVector<TSubscriber>;
  private
    FRecieveAll: boolean;
    FRecieveEnable: boolean;
    FSendEnable: boolean;
    FNClass: TNClass;
    FParent: TSubscriber;
    FSubscribers: TSubsribers;
    FOnRecieve: TNotificationEvent;
    function Containing(ANotificationClass: TNClass): boolean;
    procedure NotificationRecieve(Sender: TObject; ANClass: TNClass);
    procedure NotifySubscribers(Sender: TObject; ANClass: TNClass);
  public
    constructor Create(RecieveAll: boolean = False);
    destructor Destroy; override;
    procedure Subscribe(Subscriber: TSubscriber);
    procedure UnSubscribe(Subscriber: TSubscriber);
    procedure CreateNotification(Sender: TObject; ANClass: TNClass);
  published
    property OnNotificationRecieve: TNotificationEvent write FOnRecieve;
    property Parent: TSubscriber read FParent write FParent;
    property NClass: TNClass read FNClass write FNClass;
  end;

function ToNClass(A: array of integer): TNClass;

implementation

function ToNClass(A: array of integer): TNClass;
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
  FOnRecieve := nil;
  FSubscribers := TSubsribers.Create;
  FRecieveAll := RecieveAll;
  FSendEnable := True;
  FRecieveEnable := True;
end;

destructor TSubscriber.Destroy;
begin
  if FParent <> nil then
    FParent.UnSubscribe(Self);
  FSubscribers.Free;
  inherited Destroy;
end;

function TSubscriber.Containing(ANotificationClass: TNClass): boolean;
var
  i: integer;
  j: integer;
begin
  if FRecieveAll then
    Exit(True);
  if not FRecieveEnable then
    Exit(False);
  for i := 0 to Length(FNClass) - 1 do
    for j := 0 to Length(ANotificationClass) - 1 do
      if (FNClass[i] = ANotificationClass[j]) then
        Exit(True);
  Exit(False);
end;

procedure TSubscriber.Subscribe(Subscriber: TSubscriber);
begin
  FSubscribers.PushBack(Subscriber);
  Subscriber.Parent := Self;
end;

procedure TSubscriber.UnSubscribe(Subscriber: TSubscriber);
var
  Index: integer;
begin
  Index := FSubscribers.FindInd(Subscriber);
  if Index <> -1 then
    FSubscribers.DeleteInd(Index);
end;

procedure TSubscriber.CreateNotification(Sender: TObject; ANClass: TNClass);
var
  Iterator: TSubscriber;
begin
  Iterator := Self;
  while (Iterator.FParent <> nil) and (Iterator.FParent.Containing(NClass)) do
    Iterator := Iterator.FParent;
  Iterator.NotifySubscribers(Sender, ANClass);
end;

procedure TSubscriber.NotificationRecieve(Sender: TObject; ANClass: TNClass);
begin
  if not FRecieveEnable then
    Exit;
  if FOnRecieve <> nil then
    FOnRecieve(Sender);
  NotifySubscribers(Sender, ANClass);
end;

procedure TSubscriber.NotifySubscribers(Sender: TObject; ANClass: TNClass);
var
  i: integer;
begin
  for i := 0 to FSubscribers.Size - 1 do
    if FSubscribers[i].Containing(ANClass) then
      FSubscribers[i].NotificationRecieve(Sender, ANClass);
end;

end.
