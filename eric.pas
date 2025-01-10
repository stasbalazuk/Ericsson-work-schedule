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
  FMX.TabControl, FMX.DialogService, DW.AlertDialog, FMX.DateTimeCtrls;

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
    Label1: TLabel;  //    DateView1: TDateView;
    Timer1: TTimer;
    chkMessage: TCheckBox;
    NotificationCenter1: TNotificationCenter;
    btnExit: TButton;
    tmr1: TTimer;
    DateView1: TDateEdit;
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
    procedure tmr1Timer(Sender: TObject);
  private
    { Private declarations }
    indMess: Integer;
    idx: Integer; uTimeWork: string;
    WDay: TDateTime;
    StartDate: TDateTime;  // Дата начала работы
    EndDate: TDateTime;   // Дата окончания работы
    Schedule: TMonthScheduleList;
    myYear, myMonth, myDay : Word;
    CurrentDay: TDateTime; CntDay: Integer;
    procedure MyStart;
    procedure GetLogData;
    procedure ShowMessageDialog(msg: string);
    procedure DisplayScheduleInGrid(Schedule: TMonthScheduleList; StringGrid: TStringGrid);
    procedure InitializeLogProcessing;
    procedure ProcessWorkSchedule(const WorkSchedule: TWorkDayList; myY, myM, myD: Word; var x: Integer);
    procedure UpdateShiftNames(var WorkDay: TWorkDay; x: Integer);
    function IsTodayWorkDay(const WorkDay: TWorkDay; myY, myM, myD: Word): Boolean;
    procedure HandleTodayWorkDay(const WorkDay: TWorkDay);
    procedure AddWorkDayToMemo(const WorkDay: TWorkDay);
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
  x: Integer; xStr: string;
begin
  x := 0;
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
      if x >= 2 then x := 0;
      if WorkDay.Shift = 'A (Day shift)' then begin
         Inc(x); xStr := IntToStr(x);
         Result[Result.Count - 1].DaysA.Add(Format('%s %s - %s',
          [DateToStr(WorkDay.Date), TimeToStr(WorkDay.StartTime), TimeToStr(WorkDay.EndTime)]));
      end else begin
         Inc(x); xStr := IntToStr(x);
      if WorkDay.Shift = 'C (Night Shift)' then
         Result[Result.Count - 1].DaysC.Add(Format('%s %s - %s',
          [DateToStr(WorkDay.Date), TimeToStr(WorkDay.StartTime), TimeToStr(WorkDay.EndTime)]));
      end;
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
  CurrentDate, EntryDate: TDateTime;
  DayInfo: string;
begin
try
  // Очистка данных из таблицы
  StringGrid.RowCount := 1; // Устанавливаем одну строку для заголовков
  ClearStringGrid(StringGrid); // Очищаем существующий контент

  // Проверка, есть ли данные в расписании
  if Schedule.Count = 0 then
  begin
    ShowMessage('There is no data to display.'); // Уведомляем пользователя
    Exit; // Завершаем процедуру
  end;

  // Устанавливаем текущую дату
  CurrentDate := Date;
  Row := 1; // Начинаем добавление данных со второй строки (после заголовка)

  // Проходим по расписанию
  for var DayIndex := 0 to Schedule.Count - 1 do
  begin
    // Разделяем строки смен DaysA и DaysC
    DaysAList := Schedule[DayIndex].DaysA.Text.Split([sLineBreak], TStringSplitOptions.ExcludeEmpty);
    DaysCList := Schedule[DayIndex].DaysC.Text.Split([sLineBreak], TStringSplitOptions.ExcludeEmpty);

    for AIndex := 0 to High(DaysAList) do
    begin
      DayInfo := DaysAList[AIndex];

      // Извлекаем дату из строки
      if TryStrToDate(Copy(DayInfo, 1, 10), EntryDate) and (EntryDate >= CurrentDate) then
      begin
        // Убедимся, что таблица имеет достаточно строк
        if Row >= StringGrid.RowCount then
          StringGrid.RowCount := Row + 1;

        // Устанавливаем текущий месяц в первую колонку
        StringGrid.Cells[0, Row] := Schedule[DayIndex].Month;

        // Устанавливаем данные для смены A
        StringGrid.Cells[1, Row] := DayInfo;

        // Устанавливаем данные для смены C (если доступно)
        if AIndex < Length(DaysCList) then
          StringGrid.Cells[2, Row] := DaysCList[AIndex]
        else
          StringGrid.Cells[2, Row] := '-'; // Заполняем пустую смену C

        Inc(Row); // Переходим к следующей строке
      end;
    end;
  end;
  // Проверка, были ли добавлены данные
  if Row = 1 then
    ShowMessage('No data for current or future date.');
except
  Exit;
end;
end;

procedure TfEric.MyStart;
var
  WorkSchedule: TWorkDayList;
  OrganizedSchedule: TMonthScheduleList;
begin
  try
  // Получаем расписание с нужными датами
    WorkSchedule := GetWorkSchedule(StartDate, EndDate);  //  WorkSchedule := GetWorkSchedule(Now, EndDate);
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
  DaysOfWeek: array[1..7] of string = ('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday');
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

//===============================================
procedure TfEric.InitializeLogProcessing;
begin
  Memo1.Lines.Clear;
  Timer1.Enabled := False;
  uTimeWork := '';
end;

procedure TfEric.GetLogData;
var
  WorkSchedule: TWorkDayList;
  myY, myM, myD: Word;
  x: Integer;
begin
  WorkSchedule := GetWorkSchedule(StartDate, EndDate);
  try
    InitializeLogProcessing;
    DecodeDate(Now, myY, myM, myD);
    ProcessWorkSchedule(WorkSchedule, myY, myM, myD, x);
  finally
    WorkSchedule.Free;
  end;
end;

procedure TfEric.ProcessWorkSchedule(const WorkSchedule: TWorkDayList; myY, myM, myD: Word; var x: Integer);
var
  i: Integer;
  WorkDay: TWorkDay;
  CurrentDate, WorkDate: TDateTime;
begin
  x := 0;
  CurrentDate := EncodeDate(myY, myM, myD); // Текущая дата

  for i := 0 to WorkSchedule.Count - 1 do
  begin
    WorkDay := WorkSchedule[i];
    WorkDate := WorkDay.Date; // Предполагаем, что WorkDay имеет поле Date типа TDateTime

    // Пропускаем дни, которые уже прошли
    if WorkDate < CurrentDate then
      Continue;

    // Смены
    Inc(x);
    if x > 2 then
      x := 1;

    UpdateShiftNames(WorkDay, x);

    // Обрабатываем только текущий или будущий рабочий день
    if IsTodayWorkDay(WorkDay, myY, myM, myD) then
      HandleTodayWorkDay(WorkDay);

    AddWorkDayToMemo(WorkDay); // Добавляем в Memo только с текущей даты и далее
  end;
end;

procedure TfEric.UpdateShiftNames(var WorkDay: TWorkDay; x: Integer);
var
  xStr: string;
begin
  xStr := IntToStr(x);
  if Pos('A (Day shift)', WorkDay.Shift) > 0 then
    WorkDay.Shift := 'A' + xStr + ' (Day shift)';
  if Pos('C (Night Shift)', WorkDay.Shift) > 0 then
    WorkDay.Shift := 'C' + xStr + ' (Night Shift)';
  if Pos('Day off', WorkDay.Shift) > 0 then
    WorkDay.Shift := 'Relaxing at home';
end;

function TfEric.IsTodayWorkDay(const WorkDay: TWorkDay; myY, myM, myD: Word): Boolean;
var
  todayStr: string;
begin
  todayStr := FormatDateTime('dd.mm.yyyy', EncodeDate(myY, myM, myD));
  Result := todayStr = DateToStr(WorkDay.Date);
end;

procedure TfEric.HandleTodayWorkDay(const WorkDay: TWorkDay);
begin
  uTimeWork := TimeToStr(WorkDay.StartTime);
  Label1.Text := Format('Today: %s %s, Time: %s', [DateToStr(WorkDay.Date), WorkDay.Shift, uTimeWork]);
  Timer1.Enabled := True;
end;

procedure TfEric.AddWorkDayToMemo(const WorkDay: TWorkDay);
var
  dateStr, dayOfWeek, timeRange: string;
begin
  dateStr := DateToStr(WorkDay.Date);
  dayOfWeek := GetDayOfWeek(WorkDay.Date);
  if WorkDay.Shift = 'Relaxing at home' then
    Memo1.Lines.Add(Format('Date: %s [%s], %s', [dateStr, dayOfWeek, WorkDay.Shift]))
  else
  begin
    timeRange := Format('%s - %s', [TimeToStr(WorkDay.StartTime), TimeToStr(WorkDay.EndTime)]);
    Memo1.Lines.Add(Format('Date: %s [%s], %s, Time: %s', [dateStr, dayOfWeek, WorkDay.Shift, timeRange]));
  end;
end;
//===============================================
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
    idx := 5;
    indMess := 3;
    statFrm := False;
    WDay := Trunc(Now);
    CurrentDay := Trunc(Now);
    CntDay := Trunc(WDay - CurrentDay);
    // Устанавливаем начальную и конечную дату
    DateView1.Date := Now+366;
    DecodeDate(DateView1.Date, myYear, myMonth, myDay);
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
  MyStart;
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
  WorkDayDate: TDateTime;
  i: Integer;
  IsColored: Boolean;
begin
  try
    if Schedule = nil then
      Exit;

    StartOfMonth := StartOfTheMonth(Now);
    EndOfMonth := EndOfTheMonth(Now);
    CurrentDay := Trunc(Now);
    IsColored := False;

    if Column.Index in [1, 2] then // Проверка для смен A и C
    begin
      if (Row - 1 >= 0) and (Row - 1 < Schedule.Count) then
      begin
        // Выбираем нужный список смены (DaysA или DaysC) в зависимости от столбца
        if Column.Index = 1 then
          DaysList := Schedule[Row - 1].DaysA.Text.Split([#$D#$A])
        else
          DaysList := Schedule[Row - 1].DaysC.Text.Split([#$D#$A]);

        for i := 0 to High(DaysList) do
        begin
          CleanDateStr := TrimLeft(DaysList[i]);
          if Pos(' ', CleanDateStr) > 0 then
            CleanDateStr := Copy(CleanDateStr, 0, Pos(' ', CleanDateStr) - 1);
          if Trim(CleanDateStr) = '' then
            Continue;

          try
            WorkDayDate := StrToDate(CleanDateStr);

            // Проверяем, попадает ли дата в текущий месяц
            if (WorkDayDate >= StartOfMonth) and (WorkDayDate <= EndOfMonth) then
            begin
              DaysLeft := Trunc(WorkDayDate - CurrentDay);

              if DaysLeft = 0 then
              begin
                Canvas.Fill.Color := TAlphaColorRec.Red; // Сегодня
                IsColored := True;
                Break;
              end
              else if DaysLeft > 0 then
              begin
                if DaysLeft <= 1 then
                  Canvas.Fill.Color := TAlphaColorRec.Red // Завтра
                else if DaysLeft <= 3 then
                  Canvas.Fill.Color := TAlphaColorRec.Yellow // Ближайшие 3 дня
                else
                  Canvas.Fill.Color := TAlphaColorRec.Green; // В будущем

                IsColored := True;
                Break; // Останавливаем обработку на первом найденном рабочем дне
              end;
            end;
          except
            on E: EConvertError do
              Canvas.Fill.Color := TAlphaColorRec.White;
          end;
        end;

        if not IsColored then
          Canvas.Fill.Color := TAlphaColorRec.White; // Если ничего не нашли
      end
      else
        Canvas.Fill.Color := TAlphaColorRec.White;

      // Отрисовываем фон ячейки
      Canvas.FillRect(Bounds, 0, 0, AllCorners, 1);
    end;

    // Отрисовываем текст
    Canvas.Fill.Color := TAlphaColorRec.Black; // Черный цвет текста
    Canvas.FillText(Bounds, Value.ToString, True, 1, [], TTextAlign.Leading, TTextAlign.Center);
  except
    on E: Exception do
      Exit;
  end;
end;

procedure TfEric.Timer1Timer(Sender: TObject);
begin
  idx := idx - 1;
  Label1.Text := Format('%s, Number of days left: [%d] - %d', [DateToStr(Now), CntDay, idx]);

  if idx <= 0 then
  begin
    try
      idx := 10; // Сброс индекса
      MyStart;

      // Текущий день
      CurrentDay := Trunc(Now);

      // Расчёт количества дней до рабочего дня
      CntDay := Trunc(WDay - CurrentDay);

      // Устанавливаем начальную и конечную дату для DateView1
      DateView1.DateTime := Now + 366;
      DecodeDate(DateView1.DateTime, myYear, myMonth, myDay);

      // Проверяем, включены ли уведомления
      if chkMessage.IsChecked then
      begin
        if CntDay = 1 then
        begin
          ShowMessageDialog(
            Format('There is %d day left until the work shift. [%s]', [CntDay, DateToStr(WDay)])
          );
        end
        else if CntDay = 3 then
        begin
          ShowMessageDialog(
            Format('There are %d days left until the work shift. [%s]', [CntDay, DateToStr(WDay)])
          );
        end;
      end;

      // Обновляем текст метки
      Label1.Text := Format('%s, Number of days left: [%d]', [DateToStr(Now), CntDay]);

      // Отключаем таймер, если дни закончились
      if CntDay <= 0 then
      begin
        Timer1.Enabled := False;
        Label1.Text := 'The work shift has started or already passed.';
      end;
    except
      on E: Exception do
      begin
        Label1.Text := 'Error: ' + E.Message;
      end;
    end;
  end;
end;

procedure TfEric.tmr1Timer(Sender: TObject);
var
  CurrentDateTime, WorkDateTime: TDateTime;
  TimeDifference: TDateTime;
  Hours, Minutes, Seconds, Milliseconds: Word;
begin
  if Length(Trim(uTimeWork)) = 0 then
    Exit;

  try
    // Получаем текущее дату и время
    CurrentDateTime := Now;

    // Преобразуем строку uTimeWork в TTime и добавляем её к текущей дате
    WorkDateTime := Date + StrToTime(uTimeWork);

    // Считаем разницу между текущим временем и временем работы
    TimeDifference := WorkDateTime - CurrentDateTime;

    // Если время работы уже прошло, отключаем таймер
    if TimeDifference <= 0 then
    begin
      Label1.Text := 'The work shift has already begun!';
      tmr1.Enabled := False;
      Exit;
    end;

    // Декодируем разницу во времени
    DecodeTime(TimeDifference, Hours, Minutes, Seconds, Milliseconds);

    // Проверяем, сколько времени осталось до начала работы
    if (TimeDifference <= EncodeTime(2, 30, 0, 0)) and (indMess = 3) then
    begin
      Label1.Text := 'Time to get ready for work! Less than 2.30 hours left.';
      ShowNotification('Time to get ready for work! Less than 2.30 hours left.');
      Dec(indMess);
    end
    else if (TimeDifference <= EncodeTime(1, 30, 0, 0)) and (indMess = 2) then
    begin
      Label1.Text := 'Hurry up! Less than 1.30 hours left.';
      ShowNotification('Hurry up! Less than 1.30 hours left.');
      Dec(indMess);
    end
    else if (TimeDifference <= EncodeTime(0, 30, 0, 0)) and (indMess = 1) then
    begin
      Label1.Text := 'Almost time for work! Less than 30 minutes left.';
      ShowNotification('Almost time for work! Less than 30 minutes left.');
      Dec(indMess);
    end;

    // Если все сообщения показаны, отключаем таймер
    if indMess <= 0 then
    begin
      indMess := 3; // Сбрасываем счетчик для следующего дня
      tmr1.Enabled := False;
    end;
  except
    on E: Exception do
    begin
      Label1.Text := 'Error: ' + E.Message;
      tmr1.Enabled := False; // Отключаем таймер при ошибке
    end;
  end;
end;


end.
