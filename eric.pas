unit eric;

interface

uses
 // Winapi.Windows, Winapi.ShellAPI,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.PushNotification, System.Notification, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Layouts, System.Rtti, FMX.Grid.Style, FMX.Grid, DateUtils,
  UI.Base, UI.Standard, UI.Calendar, NotificationService, //Vcl.Dialogs,
  FMX.TabControl, FMX.DialogService, DW.AlertDialog;

type
  // Структура для хранения информации о рабочем дне
  TWorkDay = record
    Date: TDateTime;
    IsWorkDay: Boolean;
    Shift: string;          // 'A' для дневной смены, 'C' для ночной смены
    StartTime: TTime;
    EndTime: TTime;
  end;
  // Тип списка для хранения всех рабочих дней
  TWorkDayList = TList<TWorkDay>;

type
  TMonthSchedule = record
    Month: string;  // Название месяца
    DaysA: TStringList;  // Дни и время для смены А
    DaysC: TStringList;  // Дни и время для смены C
  end;
  TMonthScheduleList = TList<TMonthSchedule>;  // Список записей для каждого месяца

type
  TfEric = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    TabControl1: TTabControl;
    uTabl0: TTabItem;
    uTabl1: TTabItem;
    Memo1: TMemo;
    StringGrid1: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    Label1: TLabel;
    DateView1: TDateView;
    Timer1: TTimer;
    chkMessage: TCheckBox;
    NotificationCenter1: TNotificationCenter;
    btnExit: TButton;
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1DrawColumnCell(Sender: TObject; const Canvas: TCanvas;
      const Column: TColumn; const Bounds: TRectF; const Row: Integer;
      const Value: TValue; const State: TGridDrawStates);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnExitClick(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    idx: Integer;
    WorkDayDate,WDay: TDateTime;
    StartDate: TDateTime;  // Дата начала работы
    EndDate: TDateTime;   // Дата окончания работы
    Schedule: TMonthScheduleList;
    myYear, myMonth, myDay : Word;
    CurrentDay: TDateTime; CntDay: Integer;
    procedure MyStart;
    procedure GetLogData;
    procedure ShowMessageDialog(msg: string);
    procedure DisplayScheduleInGrid(Schedule: TMonthScheduleList; StringGrid: TStringGrid);
  public
    { Public declarations }
    statFrm : Boolean;
  end;

var
  fEric: TfEric;

implementation

{$R *.fmx}

procedure ShowNotification(MessageText: string);
var
  {$IF DEFINED(ANDROID) OR DEFINED(IOS)}
  NotificationCenter: TNotificationCenter;
  {$ENDIF}
  Notification: TNotification;
begin
try
  TThread.Queue(nil, // Перемещаем выполнение в основной поток
    procedure
    begin
      {$IF DEFINED(ANDROID) OR DEFINED(IOS)}
      NotificationCenter := TNotificationCenter.Create(nil); // Создание экземпляра
      try
        if NotificationCenter.Supported then
        begin
          Notification := NotificationCenter.CreateNotification;
          try
            Notification.Title := '[Attention]';
            Notification.AlertBody := MessageText;
            Notification.EnableSound := True;
            Notification.Number := 0;
            NotificationCenter.ApplicationIconBadgeNumber := 0;
            NotificationCenter.PresentNotification(Notification);
          finally
            Notification.Free;
          end;
        end
        else
        begin
          ShowMessage('Notifications are not supported on this device.');
        end;
      finally
        NotificationCenter.Free; // Освобождение ресурсов
      end;
      {$ELSE}
        TThread.Queue(nil,  // Изменение Label1 в главном потоке
          procedure
          begin
            // Проверяем состояние формы и Label
            if Assigned(fEric) and Assigned(fEric.Label1) then
            begin
              try
                fEric.Label1.Text := MessageText;
                // Проверка, что компонент TNotificationCenter доступен
                if not fEric.NotificationCenter1.Supported then
                begin
                  ShowMessage('Notifications are not supported on this platform.');
                  Exit;
                end;
                // Создаем уведомление
                Notification := fEric.NotificationCenter1.CreateNotification;
                try
                  Notification.Name := 'MyWork';
                  Notification.Title := 'Attention';
                  Notification.AlertBody := MessageText;
                  fEric.NotificationCenter1.PresentNotification(Notification);
                finally
                  Notification.Free;
                end;
              except
                on E: Exception do
                   ShowMessage('Error updating label: ' + E.Message);
              end;
            end
            else
              ShowMessage('Form or Label is not assigned');
          end);
      {$ENDIF}
    end);
except
  on E: Exception do begin
     Exit;
  end;
end;
end;

function GetWorkSchedule(StartDate: TDateTime; EndDate: TDateTime): TWorkDayList;
var
  CurrentDate: TDateTime;
  WorkDay: TWorkDay;
  IsDayShift: Boolean;
  WorkDaysCount, RestDaysCount: Integer;
begin
  // Инициализируем список для хранения расписания
  Result := TWorkDayList.Create;

  // Начинаем с дневной смены и с подсчета 4 рабочих дней
  IsDayShift := True;
  CurrentDate := StartDate;
  WorkDaysCount := 0;
  RestDaysCount := 0;

  // Проходим по каждому дню до конца года
  while CurrentDate <= EndDate do
  begin
    // Проверка, если прошло 4 рабочих дня, добавляем 4 выходных дня
    if WorkDaysCount < 4 then
    begin
      // Заполняем данные для текущего рабочего дня
      WorkDay.Date := CurrentDate;
      WorkDay.IsWorkDay := True;

      if IsDayShift then
      begin
        WorkDay.Shift := 'A (Day shift)';                   // Дневная смена
        WorkDay.StartTime := EncodeTime(7, 30, 0, 0);
        WorkDay.EndTime := EncodeTime(19, 30, 0, 0);
      end
      else
      begin
        WorkDay.Shift := 'C (Night Shift)';                   // Ночная смена
        WorkDay.StartTime := EncodeTime(19, 30, 0, 0);
        WorkDay.EndTime := EncodeTime(7, 30, 0, 0);
      end;

      // Добавляем рабочий день в список
      Result.Add(WorkDay);

      // Чередуем смену каждые два дня
      if WorkDaysCount mod 2 = 1 then
        IsDayShift := not IsDayShift;

      // Увеличиваем счетчик рабочих дней
      Inc(WorkDaysCount);
    end
    else
    begin
      // Если прошло 4 рабочих дня, добавляем выходной день
      WorkDay.Date := CurrentDate;
      WorkDay.IsWorkDay := False;
      WorkDay.Shift := 'Day off';                    // Нет смены для выходного дня
      WorkDay.StartTime := 0;
      WorkDay.EndTime := 0;

      // Добавляем выходной день в список
      Result.Add(WorkDay);

      // Увеличиваем счетчик выходных дней
      Inc(RestDaysCount);

      // Если прошло 4 выходных дня, сбрасываем счетчики и начинаем заново
      if RestDaysCount = 4 then
      begin
        WorkDaysCount := 0;
        RestDaysCount := 0;
        IsDayShift := True;  // Смена начинается снова с дневной
      end;
    end;
    // Переходим к следующему дню
    CurrentDate := CurrentDate + 1;
  end;
end;

procedure TfEric.ShowMessageDialog(msg: string);
begin
 try
   Timer1.Enabled := False;
   ShowNotification(msg);
 finally
   Timer1.Enabled := True;
 end;
end;

function OrganizeScheduleByMonth(Schedule: TWorkDayList): TMonthScheduleList;
var
  WorkDay: TWorkDay;
  CurrentMonth: string;
  MonthSchedule: TMonthSchedule;
begin
  Result := TMonthScheduleList.Create;
  for WorkDay in Schedule do
  begin
    // Получаем название текущего месяца
    CurrentMonth := FormatDateTime('mmmm', WorkDay.Date);
    // Если месяц меняется или первый элемент, добавляем новую запись в список
    if (Result.Count = 0) or (Result[Result.Count - 1].Month <> CurrentMonth) then
    begin
      MonthSchedule.Month := CurrentMonth;
      MonthSchedule.DaysA := TStringList.Create;
      MonthSchedule.DaysC := TStringList.Create;
      Result.Add(MonthSchedule);
    end;
    // Добавляем информацию о дне и времени в нужную смену
    if WorkDay.IsWorkDay then
    begin
      if WorkDay.Shift = 'A (Day shift)' then
         Result[Result.Count - 1].DaysA.Add(Format('%s %s - %s',
          [DateToStr(WorkDay.Date), TimeToStr(WorkDay.StartTime), TimeToStr(WorkDay.EndTime)]))
      else if WorkDay.Shift = 'C (Night Shift)' then
         Result[Result.Count - 1].DaysC.Add(Format('%s %s - %s',
          [DateToStr(WorkDay.Date), TimeToStr(WorkDay.StartTime), TimeToStr(WorkDay.EndTime)]));
    end;
  end;
end;
procedure ClearStringGrid(Grid: TStringGrid);
var
  I: Integer;
  J: Integer;
begin
try
  for I := 0 to Grid.ColumnCount-1 do
    for J := 0 to Grid.RowCount-1 do
      Grid.Cells[I,J] := '';
except
  Exit;
end;
end;
procedure TfEric.btnExitClick(Sender: TObject);
begin
  statFrm := True;
  Close;
end;

procedure TfEric.DisplayScheduleInGrid(Schedule: TMonthScheduleList; StringGrid: TStringGrid);
var
  Row, AIndex: Integer;
  DaysAList, DaysCList: TArray<string>;
begin
  // Clear existing data and start with one row for headers
  StringGrid.RowCount := 1; // Ensure there's a header row
  ClearStringGrid(StringGrid); // Clear any existing content in the grid

  // Check if Schedule is empty
  if Schedule.Count = 0 then
  begin
    ShowMessage('No schedule data available.'); // Notify the user
    Exit; // Exit the procedure if there are no items
  end;

  // Initialize the starting row for data
  Row := 0; // Start adding data from Row 1 (after headers)

  // Fill the grid with data from the schedule
  for var DayIndex := 0 to Schedule.Count - 1 do
  begin
    // Split the DaysA and DaysC strings into arrays
    DaysAList := Schedule[DayIndex].DaysA.Text.Split([#$D#$A]);
    DaysCList := Schedule[DayIndex].DaysC.Text.Split([#$D#$A]);

    // Ensure the StringGrid has enough rows for the new entry
    if Row >= StringGrid.RowCount then
      StringGrid.RowCount := Row + 1; // Increase row count if needed

    // Set Month for the current row
    StringGrid.Cells[0, Row] := Schedule[DayIndex].Month;

    // Populate DaysA and DaysC
    for AIndex := 0 to High(DaysAList) do
    begin
      // Ensure the StringGrid has enough rows for DaysA
      if Row >= StringGrid.RowCount then
        StringGrid.RowCount := Row + 1; // Increase row count if needed

      StringGrid.Cells[1, Row] := DaysAList[AIndex]; // Set Day A entry

      // Set Day C entry if available
      if AIndex < Length(DaysCList) then
        StringGrid.Cells[2, Row] := DaysCList[AIndex] // Set Day C entry
      else
        StringGrid.Cells[2, Row] := '-'; // Clear if no corresponding Day C entry

      Row := Row + 1; // Move to the next row for the next entry
    end;
  end;
end;

procedure TfEric.MyStart;
var
  WorkSchedule: TWorkDayList;
  OrganizedSchedule: TMonthScheduleList;
begin
  try
  // Получаем расписание с нужными датами
  WorkSchedule := GetWorkSchedule(StartDate, EndDate);
  try
    // Организуем данные по месяцам
    OrganizedSchedule := OrganizeScheduleByMonth(WorkSchedule);
    Schedule := OrganizeScheduleByMonth(WorkSchedule);
    try
      // Выводим данные в таблицу
      DisplayScheduleInGrid(OrganizedSchedule, StringGrid1);
    finally
      // Освобождаем память для списка
      for var MonthSchedule in OrganizedSchedule do
      begin
        MonthSchedule.DaysA.Free;
        MonthSchedule.DaysC.Free;
      end;
      OrganizedSchedule.Free;
    end;
    try
      GetLogData;
    except
      Memo1.Lines.Add('Error GetLogData');
    end;
  finally
    WorkSchedule.Free;
  end;
  except
    Memo1.Lines.Add('Error DisplaySchedule');
  end;
end;
function GetDayOfWeek(const uDate: TDateTime): string;
const
  DaysOfWeek: array[1..7] of string = ('Воскресенье', 'Понедельник', 'Вторник',
                                       'Среда', 'Четверг', 'Пятница', 'Суббота');
var
  Day, Month, Year: Word;
  ParsedDate: TDateTime;
begin
  // Разбираем строку даты формата 'дд.мм.гг'
  try
    DecodeDate(uDate, Year, Month, Day);
    // Формируем дату и получаем день недели
    ParsedDate := EncodeDate(Year, Month, Day);
    Result := DaysOfWeek[DayOfWeek(ParsedDate)];
  except
    on E: Exception do
       Result := 'Error data format';
  end;
end;
procedure TfEric.GetLogData;
var
  WorkSchedule: TWorkDayList;
  WorkDay: TWorkDay;
  i: Integer;
begin
  try
    WorkSchedule := GetWorkSchedule(StartDate, EndDate);
    try
      // Проходим по списку и выводим данные
      Memo1.Lines.Clear;
      for i := 0 to WorkSchedule.Count - 1 do
      begin
        WorkDay := WorkSchedule[i];
        Memo1.Lines.Add(Format('Дата: %s, Смена: %s, Время: %s - %s',
          [DateToStr(WorkDay.Date)+' ['+GetDayOfWeek(WorkDay.Date)+']', WorkDay.Shift, TimeToStr(WorkDay.StartTime), TimeToStr(WorkDay.EndTime)]));
      end;
    finally
      WorkSchedule.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Error: ' + E.Message);
  end;
end;

procedure TfEric.Label1Click(Sender: TObject);
begin
  Timer1.Enabled := True;
end;

procedure TfEric.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer1.Enabled := False;
end;

procedure TfEric.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := statFrm;
  Timer1.Enabled := False;
  Application.ShowHint := False;
end;

procedure TfEric.FormCreate(Sender: TObject);
begin
  try
    idx := 10;
    statFrm := False;
    WDay := Trunc(Now);
    CurrentDay := Trunc(Now);
    CntDay := Trunc(WDay - CurrentDay);
    // Устанавливаем начальную и конечную дату
    DateView1.DateTime := Now+366;
    DecodeDate(DateView1.DateTime, myYear, myMonth, myDay);
    StartDate := EncodeDate(2024, 11, 9);  // Дата начала работы
    EndDate := EncodeDate(myYear, myMonth, myDay);   // Дата окончания работы
    Label1.Text := DateToStr(Now) + ', Number of days left: ['+IntToStr(CntDay)+']';
  except
    on E: Exception do
       ShowMessage('Error: ' + E.Message);
  end;
end;

procedure TfEric.FormDestroy(Sender: TObject);
begin
try
  if Assigned(Schedule) then Schedule.Free; statFrm := True; Application.Terminate;
except
  Exit;
end;
end;

procedure TfEric.FormShow(Sender: TObject);
begin
  {$IF DEFINED(ANDROID) OR DEFINED(IOS)}
  TabControl1.Tabs[0].Visible := False;
  TabControl1.Tabs[1].Visible := True;
  TabControl1.GotoVisibleTab(1);
  TabControl1.Tabs[1].SetFocus;
  {$ENDIF}
end;

procedure TfEric.StringGrid1DrawColumnCell(Sender: TObject;
  const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
  const Row: Integer; const Value: TValue; const State: TGridDrawStates);
var
  DaysLeft: Integer;
  DaysList: TArray<string>;
  CleanDateStr: string;
  StartOfMonth: TDateTime;
  EndOfMonth: TDateTime;
  CurrentDay: TDateTime;
  i: Integer;
  IsColored: Boolean;
begin
try
  if Schedule = nil then Exit;

  StartOfMonth := StartOfTheMonth(Now);
  EndOfMonth := EndOfTheMonth(Now);
  CurrentDay := Trunc(Now);

  if Column.Index in [1, 2] then // Проверка для смен A и C
  begin
    if (Row - 1 >= 0) and (Row - 1 < Schedule.Count) then
    begin
      // Выбираем нужный список смены (DaysA или DaysC) в зависимости от столбца
      if Column.Index = 1 then
         DaysList := Schedule[Row - 1].DaysA.Text.Split([#$D#$A])
      else
         DaysList := Schedule[Row - 1].DaysC.Text.Split([#$D#$A]);
         IsColored := False;
      for i := 0 to High(DaysList) do
      begin
          CleanDateStr := TrimLeft(DaysList[i]);
        if Pos(' ', CleanDateStr) > 0 then
           CleanDateStr := Copy(CleanDateStr, 0, Pos(' ', CleanDateStr) - 1);
        if Trim(CleanDateStr) = '' then Break;
        try
           WorkDayDate := StrToDate(CleanDateStr);
          // Проверяем, попадает ли дата в текущий месяц
          if (WorkDayDate >= StartOfMonth) and (WorkDayDate <= EndOfMonth) then
          begin
            DaysLeft := Trunc(WorkDayDate - CurrentDay);
            if DaysLeft <= 0 then
               Canvas.Fill.Color := TAlphaColorRec.Red  // Если день сегодня
            else if DaysLeft > 0 then
            begin
              if DaysLeft <= 1 then begin
                 Canvas.Fill.Color := TAlphaColorRec.Red;
                 WDay := WorkDayDate;
              end else if DaysLeft <= 3 then begin
                 Canvas.Fill.Color := TAlphaColorRec.Yellow;
                 WDay := WorkDayDate;
              end else
                Canvas.Fill.Color := TAlphaColorRec.Green;
            end
            else
              Canvas.Fill.Color := TAlphaColorRec.White;
              IsColored := True;
              Break; // Останавливаемся, если нашли рабочий день для этой ячейки
          end;
        except
          on E: EConvertError do
             Canvas.Fill.Color := TAlphaColorRec.White;
        end;
      end;
      if not IsColored then
         Canvas.Fill.Color := TAlphaColorRec.White;
    end
    else
      Canvas.Fill.Color := TAlphaColorRec.White;
      Label1.Text := DateToStr(CurrentDay);
      // Отрисовываем фон ячейки
      Canvas.FillRect(Bounds, 0, 0, AllCorners, 1);
  end;
  Canvas.Fill.Color := TAlphaColorRec.Black; // Черный цвет текста
  Canvas.FillText(Bounds, Value.ToString, True, 1, [], TTextAlign.Leading, TTextAlign.Center);
except
  Exit;
end;
end;

procedure TfEric.Timer1Timer(Sender: TObject);
begin
  idx := idx - 1;
  Label1.Text := DateToStr(Now) + ', Number of days left: ['+IntToStr(CntDay)+'] - '+IntToStr(idx);
  if idx <= 0 then begin
     try
      idx := 10;
      MyStart;
      CurrentDay := Trunc(Now);
      CntDay := Trunc(WDay - CurrentDay);
      // Устанавливаем начальную и конечную дату
      DateView1.DateTime := Now+366;
      DecodeDate(DateView1.DateTime, myYear, myMonth, myDay);
      if chkMessage.IsChecked then begin
      if CntDay = 3 then ShowMessageDialog('There are '+IntToStr(CntDay)+' days left until the work shift. ['+DateToStr(WDay)+']');
      if CntDay = 1 then ShowMessageDialog('There are '+IntToStr(CntDay)+' days left until the work shift. ['+DateToStr(WDay)+']');
      end;
      Label1.Text := DateToStr(Now) + ', Number of days left: ['+IntToStr(CntDay)+']';
      Timer1.Enabled := False;
     except
      on E: Exception do
         ShowMessage('Error: ' + E.Message);
     end;
  end;
end;

end.
