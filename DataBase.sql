CREATE DATABASE 'localhost:C:\myDb\DataBase.fdb' USER 'SYSDBA' PASSWORD 'masterkey' DEFAULT CHARACTER SET UTF8;
CREATE GENERATOR GroupsIDGenerator;
CREATE GENERATOR LessonsIDGenerator;
CREATE GENERATOR TeachersIDGenerator;
CREATE GENERATOR ClassRoomsIDGenerator;
CREATE GENERATOR LessonsTimesIDGenerator;
CREATE GENERATOR WeekDaysIDGenerator;
CREATE GENERATOR TimeTableIDGenerator;
CREATE GENERATOR LessonsTypesIDGenerator;

CREATE TABLE Groups
(
   ID           INTEGER NOT NULL PRIMARY KEY
  ,Name         VARCHAR(10)
  ,Size         INTEGER
  ,StartingDate TIMESTAMP
  ,EndingDate   TIMESTAMP
);

CREATE TABLE Lessons
(
   ID           INTEGER NOT NULL PRIMARY KEY
  ,Name         VARCHAR(100)
  ,StartingDate TIMESTAMP
  ,EndingDate   TIMESTAMP
);

CREATE TABLE Teachers
(
   ID           INTEGER NOT NULL PRIMARY KEY
  ,LastName     VARCHAR(20)
  ,FirstName    VARCHAR(20)
  ,MiddleName   VARCHAR(20)
  ,StartingDate TIMESTAMP
  ,EndingDate   TIMESTAMP
);

CREATE TABLE ClassRooms
(
   ID   INTEGER NOT NULL PRIMARY KEY
  ,Name VARCHAR(10)
  ,Size INTEGER
);

CREATE TABLE LessonTimes
(
   ID           INTEGER NOT NULL PRIMARY KEY
  ,StartingTime TIME
  ,EndingTime   TIME
);

CREATE TABLE WeekDays
(
   ID   INTEGER NOT NULL PRIMARY KEY
  ,Name VARCHAR(20)
);

CREATE TABLE LessonTypes
(
   ID   INTEGER NOT NULL PRIMARY KEY
  ,Name VARCHAR(50)
);

CREATE TABLE TimeTable
(
   ID           INTEGER NOT NULL PRIMARY KEY
  ,LessonID     INTEGER REFERENCES Lessons(ID)
  ,LessonTypeID INTEGER REFERENCES LessonTypes(ID)
  ,TeacherID    INTEGER REFERENCES Teachers(ID)
  ,GroupID      INTEGER REFERENCES Groups(ID)
  ,ClassRoomID  INTEGER REFERENCES ClassRooms(ID)
  ,WeekDayID    INTEGER REFERENCES WeekDays(ID)
  ,LessonTimeID INTEGER REFERENCES LessonTimes(ID)
);

SET TERM ^ ;

CREATE TRIGGER GroupsIDTrigger FOR Groups ACTIVE BEFORE INSERT
AS BEGIN
  new.ID = NEXT VALUE FOR GroupsIDGenerator;
END^

CREATE TRIGGER TeachersIDTrigger FOR Teachers ACTIVE BEFORE INSERT
AS BEGIN
  new.ID = NEXT VALUE FOR TeachersIDGenerator;
END^

CREATE TRIGGER LessonsIDTrigger FOR Lessons ACTIVE BEFORE INSERT
AS BEGIN
  new.ID = NEXT VALUE FOR LessonsIDGenerator;
END^

CREATE TRIGGER LessonsTimesIDTrigger FOR LessonTimes ACTIVE BEFORE INSERT
AS BEGIN
  new.ID = NEXT VALUE FOR LessonsTimesIDGenerator;
END^

CREATE TRIGGER WeekDaysIDTrigger FOR WeekDays ACTIVE BEFORE INSERT
AS BEGIN
  new.ID = NEXT VALUE FOR WeekDaysIDGenerator;
END^

CREATE TRIGGER ClassRoomsIDTrigger FOR ClassRooms ACTIVE BEFORE INSERT
AS BEGIN
  new.ID = NEXT VALUE FOR ClassRoomsIDGenerator;
END^

CREATE TRIGGER LessonsTypesIDTrigger FOR LessonTypes ACTIVE BEFORE INSERT
AS BEGIN
  new.ID = NEXT VALUE FOR LessonsTypesIDGenerator;
END^

CREATE TRIGGER TimeTableIDTrigger FOR Timetable ACTIVE BEFORE INSERT
AS BEGIN
  new.ID = NEXT VALUE FOR TimeTableIDGenerator;
END^

SET TERM ; ^

COMMIT;
INSERT INTO Groups VALUES(1 ,'Б8103а1' ,14 ,'01.09.2015' ,'01.07.2018');
INSERT INTO Groups VALUES(2 ,'Б8103а2' ,19 ,'01.09.2015' ,'01.07.2018');
INSERT INTO Groups VALUES(3 ,'Б8103б1' ,23 ,'01.09.2015' ,'01.07.2019');
INSERT INTO Groups VALUES(4 ,'Б8103б2' ,17 ,'01.09.2015' ,'01.07.2019');
INSERT INTO Groups VALUES(5 ,'Б8102-1' ,16 ,'01.09.2015' ,'10.07.2021');
INSERT INTO Groups VALUES(6 ,'Б8102-2' ,22 ,'01.09.2015' ,'10.07.2021');
INSERT INTO Groups VALUES(7 ,'С3107в'  ,27 ,'01.07.2015' ,'21.08.2015');

INSERT INTO Lessons VALUES(1  ,'Математический анализ'                         ,'01.09.2007' ,'01.01.2020');
INSERT INTO Lessons VALUES(2  ,'Дискретная математика и математическая логика' ,'01.09.2014' ,'01.01.2020');
INSERT INTO Lessons VALUES(3  ,'Алгебра и геометрия'                           ,'01.07.2003' ,'01.01.2020');
INSERT INTO Lessons VALUES(4  ,'Иностранный язык'                              ,'01.07.2011' ,'01.01.2020');
INSERT INTO Lessons VALUES(5  ,'Практикум на ЭВМ'                              ,'01.09.2008' ,'01.01.2020');
INSERT INTO Lessons VALUES(6  ,'Языки и методы программирования'               ,'01.09.2013' ,'01.01.2020');
INSERT INTO Lessons VALUES(7  ,'Базы данных'                                   ,'01.09.2011' ,'01.01.2020');
INSERT INTO Lessons VALUES(8  ,'Архитектура компьютеров'                       ,'01.07.2007' ,'01.01.2020');
INSERT INTO Lessons VALUES(9  ,'Элективные курсы по физической культуре'       ,'01.09.2008' ,'01.01.2020');
INSERT INTO Lessons VALUES(10 ,'Software engineering'                          ,'01.09.2014' ,'01.01.2020');
INSERT INTO Lessons VALUES(11 ,'Теоретическая математика и физика'             ,'01.09.2013' ,'01.01.2020');
INSERT INTO Lessons VALUES(12 ,'Русский язык и культура речи'                  ,'01.09.2007' ,'01.01.2020');
INSERT INTO Lessons VALUES(13 ,'Алгебра'                                       ,'01.09.2013' ,'01.01.2020');
INSERT INTO Lessons VALUES(14 ,'История'                                       ,'01.01.2007' ,'01.01.2020');
INSERT INTO Lessons VALUES(15 ,'Теоретическая механика'                        ,'01.01.2011' ,'01.01.2020');
INSERT INTO Lessons VALUES(16 ,'Начертательная геометрия и инженерная графика' ,'01.09.2008' ,'01.01.2020');
INSERT INTO Lessons VALUES(17 ,'Физика'                                        ,'01.09.2007' ,'01.01.2020');
INSERT INTO Lessons VALUES(18 ,'Информационные технологии в строительстве'     ,'01.09.2011' ,'01.01.2020');
INSERT INTO Lessons VALUES(19 ,'Инженерная геология'                           ,'01.09.2011' ,'01.01.2020');

INSERT INTO Teachers VALUES(1  ,'Кленин'        ,'Александр' ,'Сергеевич'      ,'10.05.2000' ,'01.01.2020');
INSERT INTO Teachers VALUES(2  ,'Клевчихин'     ,'Юрий'      ,'Александрович'  ,'01.09.2004' ,'01.01.2020');
INSERT INTO Teachers VALUES(3  ,'Прилепкина'    ,'Елена'     ,'Гумаровна'      ,'12.05.2004' ,'01.01.2020');
INSERT INTO Teachers VALUES(4  ,'Спорышев'      ,'Максим'    ,'Сергеевич'      ,'12.05.2004' ,'01.01.2020');
INSERT INTO Teachers VALUES(5  ,'Машенцев'      ,'Владимир'  ,'Юрьевич'        ,'08.03.2000' ,'01.01.2020');
INSERT INTO Teachers VALUES(6  ,'Пак'           ,'Геннадий'  ,'Константинович' ,'04.05.2000' ,'01.01.2020');
INSERT INTO Teachers VALUES(7  ,'Трикашная'     ,'Наталья'   ,'Вячеславовна'   ,'01.09.2004' ,'01.01.2020');
INSERT INTO Teachers VALUES(8  ,'Кириленко'     ,'Лариса'    ,'Михайловна'     ,'01.09.2004' ,'01.01.2020');
INSERT INTO Teachers VALUES(9  ,'Баранов'       ,'Андрей'    ,'Александрович'  ,'01.09.2009' ,'01.01.2020');
INSERT INTO Teachers VALUES(10 ,'Алексанин'     ,'Григорий'  ,'Анатольевич'    ,'08.03.2000' ,'01.01.2020');
INSERT INTO Teachers VALUES(11 ,'Веремеева'     ,'Ирина'     ,'Феликсовна'     ,'01.09.2004' ,'01.01.2020');
INSERT INTO Teachers VALUES(12 ,'Абстрактный'   ,'Учитель'   ,'Физкультуры'    ,'08.05.2009' ,'01.01.2020');
INSERT INTO Teachers VALUES(13 ,'Пак'           ,'С.'        ,'Б.'             ,'01.09.2007' ,'01.01.2020');
INSERT INTO Teachers VALUES(14 ,'Сущенко'       ,'А.'        ,'А.'             ,'01.09.2009' ,'01.01.2020');
INSERT INTO Teachers VALUES(15 ,'Малыкина'      ,'Ирина'     ,'Анатольевна'    ,'01.09.2000' ,'01.01.2020');
INSERT INTO Teachers VALUES(16 ,'Чеканов'       ,'Сергей'    ,'Геннадьевич'    ,'01.01.2007' ,'01.01.2020');
INSERT INTO Teachers VALUES(17 ,'Мишаков'       ,'Александр' ,'Владиславович'  ,'01.01.2007' ,'01.01.2020');
INSERT INTO Teachers VALUES(18 ,'Сторожок'      ,'Евгений'   ,'Анатольевич'    ,'01.01.2000' ,'01.01.2020');
INSERT INTO Teachers VALUES(19 ,'Перцевая'      ,'Ксения'    ,'Александровна'  ,'01.03.2000' ,'01.01.2020');
INSERT INTO Teachers VALUES(20 ,'Степанова'     ,'Алена'     ,'Андреевна'      ,'08.01.2005' ,'01.01.2020');
INSERT INTO Teachers VALUES(21 ,'Сакун'         ,'Р.'        ,'А.'             ,'01.09.2005' ,'01.01.2020');
INSERT INTO Teachers VALUES(22 ,'Крикунова'     ,'Юлия'      ,'Анатольевна'    ,'15.01.2003' ,'01.01.2020');
INSERT INTO Teachers VALUES(23 ,'Исхакова'      ,'Ольга'     ,'Дмитриевна'     ,'15.05.2003' ,'01.01.2020');
INSERT INTO Teachers VALUES(24 ,'Бойко'         ,'Людмила'   ,'Александровна'  ,'01.09.2000' ,'01.01.2020');
INSERT INTO Teachers VALUES(25 ,'Шустикова'     ,'Татьяна'   ,'Валентиновна'   ,'08.01.2005' ,'01.01.2020');
INSERT INTO Teachers VALUES(26 ,'Мацуцин'       ,'Артур'     ,'Алексеевич'     ,'15.03.2000' ,'01.01.2020');
INSERT INTO Teachers VALUES(27 ,'Фоменко'       ,'Елена'     ,'Львовна'        ,'15.01.2000' ,'01.01.2020');
INSERT INTO Teachers VALUES(28 ,'Проценко'      ,'Виктория'  ,'Владимировна'   ,'01.09.2003' ,'01.01.2020');
INSERT INTO Teachers VALUES(29 ,'Василенко'     ,'Геннадий'  ,'Петрович'       ,'01.09.2000' ,'01.01.2020');
INSERT INTO Teachers VALUES(30 ,'Аверкова'      ,'Галина'    ,'Владимировны'   ,'01.03.2000' ,'01.01.2020');
INSERT INTO Teachers VALUES(31 ,'Руковишникова' ,'Валентина' ,'Ивановна'       ,'01.05.2000' ,'01.01.2020');

INSERT INTO LessonTimes VALUES(1 ,'08:30:00.0000' ,'10:00:00.0000');
INSERT INTO LessonTimes VALUES(2 ,'10:10:00.0000' ,'11:40:00.0000');
INSERT INTO LessonTimes VALUES(3 ,'11:50:00.0000' ,'13:20:00.0000');
INSERT INTO LessonTimes VALUES(4 ,'13:30:00.0000' ,'15:00:00.0000');
INSERT INTO LessonTimes VALUES(5 ,'15:10:00.0000' ,'16:40:00.0000');
INSERT INTO LessonTimes VALUES(6 ,'16:50:00.0000' ,'18:20:00.0000');
INSERT INTO LessonTimes VALUES(7 ,'18:30:00.0000' ,'20:00:00.0000');
INSERT INTO LessonTimes VALUES(8 ,'20:10:00.0000' ,'21:40:00.0000');

INSERT INTO WeekDays VALUES(1 ,'Понедельник');
INSERT INTO WeekDays VALUES(2 ,'Вторник');
INSERT INTO WeekDays VALUES(3 ,'Среда');
INSERT INTO WeekDays VALUES(4 ,'Четверг');
INSERT INTO WeekDays VALUES(5 ,'Пятница');
INSERT INTO WeekDays VALUES(6 ,'Суббота');
INSERT INTO WeekDays VALUES(7 ,'Воскресенье');

INSERT INTO ClassRooms VALUES(1  ,'D733'     ,30);
INSERT INTO ClassRooms VALUES(2  ,'D733a'    ,20);
INSERT INTO ClassRooms VALUES(3  ,'D654/752' ,60);
INSERT INTO ClassRooms VALUES(4  ,'D412/542' ,60);
INSERT INTO ClassRooms VALUES(5  ,'S'        ,60);
INSERT INTO ClassRooms VALUES(6  ,'D547'     ,30);
INSERT INTO ClassRooms VALUES(7  ,'D741'     ,30);
INSERT INTO ClassRooms VALUES(8  ,'D743'     ,30);
INSERT INTO ClassRooms VALUES(9  ,'D549a'    ,20);
INSERT INTO ClassRooms VALUES(10 ,'D746'     ,30);
INSERT INTO ClassRooms VALUES(11 ,'D546'     ,30);
INSERT INTO ClassRooms VALUES(12 ,'D548'     ,30);
INSERT INTO ClassRooms VALUES(13 ,'D820'     ,30);
INSERT INTO ClassRooms VALUES(14 ,'D546а'    ,20);
INSERT INTO ClassRooms VALUES(15 ,'D734а'    ,20);
INSERT INTO ClassRooms VALUES(16 ,'D945'     ,30);
INSERT INTO ClassRooms VALUES(17 ,'D734'     ,30);
INSERT INTO ClassRooms VALUES(18 ,'C406'     ,20);
INSERT INTO ClassRooms VALUES(19 ,'C408'     ,30);
INSERT INTO ClassRooms VALUES(20 ,'E425'     ,30);
INSERT INTO ClassRooms VALUES(21 ,'E320/224' ,60);
INSERT INTO ClassRooms VALUES(22 ,'D817'     ,30);
INSERT INTO ClassRooms VALUES(23 ,'E720'     ,30);
INSERT INTO ClassRooms VALUES(24 ,'E323/236' ,60);
INSERT INTO ClassRooms VALUES(25 ,'E702/802' ,60);
INSERT INTO ClassRooms VALUES(26 ,'E809'     ,30);
INSERT INTO ClassRooms VALUES(27 ,'E706'     ,20);
INSERT INTO ClassRooms VALUES(28 ,'E809'     ,30);
INSERT INTO ClassRooms VALUES(29 ,'E541/442' ,60);
 
INSERT INTO LessonTypes VALUES(1 ,'Практическое занятие');
INSERT INTO LessonTypes VALUES(2 ,'Лекция'              );
INSERT INTO LessonTypes VALUES(3 ,'Лабораторное занятие');

INSERT INTO Timetable VALUES(1     ,1    ,2   ,3    ,1   ,3    ,1   ,2);
INSERT INTO Timetable VALUES(2     ,1    ,2   ,3    ,2   ,3    ,1   ,2);
INSERT INTO Timetable VALUES(3     ,1    ,2   ,3    ,3   ,3    ,1   ,2);
INSERT INTO Timetable VALUES(4     ,1    ,2   ,3    ,4   ,3    ,1   ,2);
INSERT INTO Timetable VALUES(5     ,2    ,2   ,6    ,1   ,4    ,1   ,3);
INSERT INTO Timetable VALUES(6     ,2    ,2   ,6    ,2   ,4    ,1   ,3);
INSERT INTO Timetable VALUES(7     ,2    ,2   ,6    ,3   ,4    ,1   ,3);
INSERT INTO Timetable VALUES(8     ,2    ,2   ,6    ,4   ,4    ,1   ,3);
INSERT INTO Timetable VALUES(9     ,3    ,2   ,6    ,1   ,4    ,1   ,4);
INSERT INTO Timetable VALUES(10    ,3    ,2   ,6    ,2   ,4    ,1   ,4);
INSERT INTO Timetable VALUES(11    ,3    ,2   ,6    ,3   ,4    ,1   ,4);
INSERT INTO Timetable VALUES(12    ,3    ,2   ,6    ,4   ,4    ,1   ,4);
INSERT INTO Timetable VALUES(13    ,9    ,1   ,12   ,1   ,5    ,1   ,5);
INSERT INTO Timetable VALUES(14    ,9    ,1   ,12   ,2   ,5    ,1   ,5);
INSERT INTO Timetable VALUES(15    ,9    ,1   ,12   ,3   ,5    ,1   ,5);
INSERT INTO Timetable VALUES(16    ,9    ,1   ,12   ,4   ,5    ,1   ,5);
INSERT INTO Timetable VALUES(17    ,1    ,1   ,2    ,1   ,6    ,2   ,1);
INSERT INTO Timetable VALUES(18    ,1    ,1   ,2    ,2   ,6    ,2   ,1);
INSERT INTO Timetable VALUES(19    ,4    ,1   ,8    ,1   ,8    ,2   ,2);
INSERT INTO Timetable VALUES(20    ,4    ,1   ,8    ,2   ,8    ,2   ,2);
INSERT INTO Timetable VALUES(21    ,4    ,1   ,11   ,1   ,7    ,2   ,2);
INSERT INTO Timetable VALUES(22    ,4    ,1   ,11   ,2   ,7    ,2   ,2);
INSERT INTO Timetable VALUES(23    ,5    ,3   ,4    ,1   ,1    ,2   ,3);
INSERT INTO Timetable VALUES(24    ,5    ,3   ,4    ,1   ,1    ,2   ,4);
INSERT INTO Timetable VALUES(25    ,6    ,3   ,9    ,2   ,2    ,2   ,3);
INSERT INTO Timetable VALUES(26    ,2    ,1   ,6    ,1   ,6    ,3   ,1);
INSERT INTO Timetable VALUES(27    ,2    ,1   ,6    ,2   ,6    ,3   ,1);
INSERT INTO Timetable VALUES(28    ,3    ,1   ,7    ,1   ,7    ,3   ,2);
INSERT INTO Timetable VALUES(29    ,3    ,1   ,7    ,2   ,7    ,3   ,2);
INSERT INTO Timetable VALUES(30    ,3    ,1   ,7    ,1   ,7    ,3   ,3);
INSERT INTO Timetable VALUES(31    ,3    ,1   ,7    ,2   ,7    ,3   ,3);
INSERT INTO Timetable VALUES(32    ,5    ,3   ,4    ,2   ,1    ,4   ,3);
INSERT INTO Timetable VALUES(33    ,5    ,3   ,4    ,2   ,1    ,4   ,4);
INSERT INTO Timetable VALUES(34    ,9    ,1   ,12   ,1   ,5    ,4   ,5);
INSERT INTO Timetable VALUES(35    ,9    ,1   ,12   ,2   ,5    ,4   ,5);
INSERT INTO Timetable VALUES(36    ,9    ,1   ,12   ,3   ,5    ,4   ,5);
INSERT INTO Timetable VALUES(37    ,9    ,1   ,12   ,4   ,5    ,4   ,5);
INSERT INTO Timetable VALUES(38    ,7    ,3   ,1    ,1   ,1    ,5   ,1);
INSERT INTO Timetable VALUES(39    ,7    ,3   ,1    ,1   ,1    ,5   ,2);
INSERT INTO Timetable VALUES(40    ,8    ,3   ,5    ,1   ,2    ,5   ,2);
INSERT INTO Timetable VALUES(41    ,8    ,3   ,5    ,1   ,2    ,5   ,3);
INSERT INTO Timetable VALUES(42    ,8    ,3   ,5    ,2   ,2    ,5   ,4);
INSERT INTO Timetable VALUES(43    ,8    ,3   ,5    ,2   ,2    ,5   ,5);
INSERT INTO Timetable VALUES(44    ,7    ,2   ,1    ,1   ,3    ,6   ,1);
INSERT INTO Timetable VALUES(45    ,6    ,2   ,1    ,1   ,3    ,6   ,2);
INSERT INTO Timetable VALUES(46    ,8    ,2   ,1    ,1   ,3    ,6   ,2);
INSERT INTO Timetable VALUES(47    ,6    ,1   ,9    ,1   ,11   ,6   ,3);
INSERT INTO Timetable VALUES(48    ,7    ,3   ,10   ,2   ,9    ,6   ,3);
INSERT INTO Timetable VALUES(49    ,7    ,3   ,10   ,2   ,9    ,6   ,4);
INSERT INTO Timetable VALUES(50    ,1    ,1   ,2    ,3   ,6    ,2   ,2);
INSERT INTO Timetable VALUES(51    ,1    ,1   ,2    ,4   ,6    ,2   ,2);
INSERT INTO Timetable VALUES(52    ,4    ,1   ,8    ,3   ,8    ,2   ,3);
INSERT INTO Timetable VALUES(53    ,4    ,1   ,8    ,4   ,8    ,2   ,3);
INSERT INTO Timetable VALUES(54    ,5    ,3   ,13   ,3   ,11   ,2   ,4);
INSERT INTO Timetable VALUES(55    ,5    ,3   ,13   ,3   ,11   ,2   ,5);
INSERT INTO Timetable VALUES(56    ,5    ,3   ,9    ,4   ,2    ,2   ,4);
INSERT INTO Timetable VALUES(57    ,5    ,3   ,9    ,4   ,2    ,2   ,5);
INSERT INTO Timetable VALUES(58    ,3    ,1   ,7    ,3   ,7    ,3   ,1);
INSERT INTO Timetable VALUES(59    ,3    ,1   ,7    ,4   ,7    ,3   ,1);
INSERT INTO Timetable VALUES(60    ,2    ,1   ,6    ,3   ,6    ,3   ,2);
INSERT INTO Timetable VALUES(61    ,2    ,1   ,6    ,4   ,6    ,3   ,2);
INSERT INTO Timetable VALUES(62    ,3    ,1   ,7    ,3   ,7    ,3   ,3);
INSERT INTO Timetable VALUES(63    ,3    ,1   ,7    ,4   ,7    ,3   ,3);
INSERT INTO Timetable VALUES(64    ,7    ,1   ,18   ,3   ,14   ,3   ,4);
INSERT INTO Timetable VALUES(65    ,7    ,1   ,18   ,4   ,14   ,3   ,4);
INSERT INTO Timetable VALUES(66    ,6    ,3   ,13   ,3   ,2    ,4   ,1);
INSERT INTO Timetable VALUES(67    ,8    ,3   ,21   ,4   ,14   ,4   ,1);
INSERT INTO Timetable VALUES(68    ,8    ,3   ,21   ,3   ,14   ,4   ,2);
INSERT INTO Timetable VALUES(69    ,7    ,3   ,18   ,3   ,14   ,4   ,2);
INSERT INTO Timetable VALUES(70    ,7    ,3   ,18   ,4   ,14   ,4   ,2);
INSERT INTO Timetable VALUES(71    ,8    ,3   ,21   ,4   ,14   ,4   ,2);
INSERT INTO Timetable VALUES(72    ,8    ,3   ,21   ,3   ,14   ,4   ,3);
INSERT INTO Timetable VALUES(73    ,6    ,3   ,9    ,4   ,2    ,4   ,3);
INSERT INTO Timetable VALUES(74    ,7    ,3   ,18   ,3   ,14   ,5   ,1);
INSERT INTO Timetable VALUES(75    ,7    ,3   ,18   ,3   ,14   ,5   ,2);
INSERT INTO Timetable VALUES(76    ,7    ,3   ,18   ,4   ,14   ,5   ,2);
INSERT INTO Timetable VALUES(77    ,7    ,3   ,18   ,4   ,14   ,5   ,3);
INSERT INTO Timetable VALUES(78    ,7    ,2   ,1    ,2   ,3    ,6   ,1);
INSERT INTO Timetable VALUES(79    ,6    ,2   ,1    ,2   ,3    ,6   ,2);
INSERT INTO Timetable VALUES(80    ,8    ,2   ,1    ,2   ,3    ,6   ,2);
INSERT INTO Timetable VALUES(81    ,7    ,2   ,1    ,3   ,3    ,6   ,1);
INSERT INTO Timetable VALUES(82    ,6    ,2   ,1    ,3   ,3    ,6   ,2);
INSERT INTO Timetable VALUES(83    ,8    ,2   ,1    ,3   ,3    ,6   ,2);
INSERT INTO Timetable VALUES(84    ,7    ,2   ,1    ,4   ,3    ,6   ,1);
INSERT INTO Timetable VALUES(85    ,6    ,2   ,1    ,4   ,3    ,6   ,2);
INSERT INTO Timetable VALUES(86    ,8    ,2   ,1    ,4   ,3    ,6   ,2);
INSERT INTO Timetable VALUES(87    ,1    ,2   ,3    ,5   ,3    ,1   ,2);
INSERT INTO Timetable VALUES(88    ,1    ,2   ,3    ,6   ,3    ,1   ,2);
INSERT INTO Timetable VALUES(89    ,10   ,1   ,14   ,5   ,1    ,1   ,3);
INSERT INTO Timetable VALUES(90    ,10   ,1   ,14   ,6   ,1    ,1   ,3);
INSERT INTO Timetable VALUES(91    ,5    ,3   ,15   ,5   ,17   ,1   ,3);
INSERT INTO Timetable VALUES(92    ,5    ,3   ,14   ,6   ,1    ,1   ,3);
INSERT INTO Timetable VALUES(93    ,5    ,3   ,15   ,5   ,17   ,1   ,4);
INSERT INTO Timetable VALUES(94    ,5    ,3   ,14   ,6   ,1    ,1   ,4);
INSERT INTO Timetable VALUES(95    ,9    ,1   ,12   ,5   ,5    ,1   ,5);
INSERT INTO Timetable VALUES(96    ,9    ,1   ,12   ,6   ,5    ,1   ,5);
INSERT INTO Timetable VALUES(97    ,2    ,1   ,16   ,5   ,12   ,2   ,2);
INSERT INTO Timetable VALUES(98    ,2    ,1   ,16   ,6   ,12   ,2   ,2);
INSERT INTO Timetable VALUES(99    ,1    ,1   ,3    ,5   ,12   ,2   ,3);
INSERT INTO Timetable VALUES(100   ,1    ,1   ,3    ,6   ,12   ,2   ,3);
INSERT INTO Timetable VALUES(101   ,4    ,1   ,8    ,5   ,8    ,2   ,4);
INSERT INTO Timetable VALUES(102   ,4    ,1   ,8    ,6   ,8    ,2   ,4);
INSERT INTO Timetable VALUES(103   ,11   ,2   ,17   ,5   ,13   ,3   ,2);
INSERT INTO Timetable VALUES(104   ,11   ,2   ,17   ,6   ,13   ,3   ,2);
INSERT INTO Timetable VALUES(105   ,11   ,1   ,17   ,5   ,13   ,3   ,3);
INSERT INTO Timetable VALUES(106   ,11   ,1   ,17   ,6   ,13   ,3   ,3);
INSERT INTO Timetable VALUES(107   ,11   ,3   ,17   ,5   ,13   ,3   ,3);
INSERT INTO Timetable VALUES(108   ,11   ,3   ,17   ,6   ,13   ,3   ,3);
INSERT INTO Timetable VALUES(109   ,7    ,2   ,18   ,5   ,14   ,3   ,4);
INSERT INTO Timetable VALUES(110   ,7    ,2   ,18   ,6   ,14   ,3   ,4);
INSERT INTO Timetable VALUES(112   ,7    ,3   ,18   ,5   ,14   ,3   ,5);
INSERT INTO Timetable VALUES(113   ,7    ,3   ,18   ,6   ,14   ,3   ,5);
INSERT INTO Timetable VALUES(114   ,6    ,3   ,14   ,5   ,1    ,4   ,2);
INSERT INTO Timetable VALUES(115   ,12   ,1   ,19   ,5   ,7    ,4   ,2);
INSERT INTO Timetable VALUES(116   ,12   ,1   ,19   ,6   ,7    ,4   ,2);
INSERT INTO Timetable VALUES(117   ,2    ,2   ,16   ,5   ,13   ,4   ,3);
INSERT INTO Timetable VALUES(118   ,2    ,2   ,16   ,6   ,13   ,4   ,3);
INSERT INTO Timetable VALUES(119   ,7    ,1   ,18   ,5   ,15   ,4   ,4);
INSERT INTO Timetable VALUES(120   ,7    ,1   ,18   ,6   ,15   ,4   ,4);
INSERT INTO Timetable VALUES(121   ,9    ,1   ,12   ,5   ,5    ,4   ,5);
INSERT INTO Timetable VALUES(122   ,9    ,1   ,12   ,6   ,5    ,4   ,5);
INSERT INTO Timetable VALUES(123   ,13   ,2   ,20   ,5   ,16   ,5   ,1);
INSERT INTO Timetable VALUES(124   ,13   ,2   ,20   ,6   ,16   ,5   ,1);
INSERT INTO Timetable VALUES(125   ,13   ,1   ,20   ,5   ,16   ,5   ,2);
INSERT INTO Timetable VALUES(126   ,13   ,1   ,20   ,6   ,16   ,5   ,2);
INSERT INTO Timetable VALUES(127   ,13   ,3   ,20   ,5   ,16   ,5   ,2);
INSERT INTO Timetable VALUES(128   ,13   ,3   ,20   ,6   ,16   ,5   ,2);
INSERT INTO Timetable VALUES(129   ,6    ,3   ,21   ,6   ,1    ,5   ,3);
INSERT INTO Timetable VALUES(130   ,6    ,2   ,1    ,5   ,3    ,6   ,2);
INSERT INTO Timetable VALUES(131   ,6    ,2   ,1    ,6   ,3    ,6   ,2);
INSERT INTO Timetable VALUES(132   ,1    ,2   ,31   ,7   ,21   ,1   ,2);
INSERT INTO Timetable VALUES(133   ,4    ,1   ,22   ,7   ,19   ,1   ,4);
INSERT INTO Timetable VALUES(134   ,14   ,1   ,23   ,7   ,19   ,2   ,1);
INSERT INTO Timetable VALUES(135   ,15   ,1   ,24   ,7   ,19   ,2   ,2);
INSERT INTO Timetable VALUES(136   ,9    ,1   ,12   ,7   ,5    ,2   ,3);
INSERT INTO Timetable VALUES(137   ,16   ,1   ,25   ,7   ,18   ,3   ,1);
INSERT INTO Timetable VALUES(138   ,16   ,1   ,25   ,7   ,18   ,3   ,2);
INSERT INTO Timetable VALUES(139   ,17   ,1   ,26   ,7   ,20   ,3   ,3);
INSERT INTO Timetable VALUES(140   ,17   ,2   ,26   ,7   ,21   ,4   ,2);
INSERT INTO Timetable VALUES(141   ,3    ,2   ,31   ,7   ,21   ,4   ,2);
INSERT INTO Timetable VALUES(142   ,17   ,3   ,27   ,7   ,22   ,4   ,3);
INSERT INTO Timetable VALUES(143   ,18   ,3   ,28   ,7   ,23   ,4   ,3);
INSERT INTO Timetable VALUES(144   ,17   ,3   ,27   ,7   ,22   ,4   ,4);
INSERT INTO Timetable VALUES(145   ,18   ,3   ,28   ,7   ,23   ,4   ,4);
INSERT INTO Timetable VALUES(146   ,14   ,2   ,23   ,7   ,24   ,5   ,2);
INSERT INTO Timetable VALUES(147   ,9    ,1   ,12   ,7   ,5    ,5   ,3);
INSERT INTO Timetable VALUES(148   ,19   ,2   ,29   ,7   ,25   ,5   ,4);
INSERT INTO Timetable VALUES(149   ,19   ,1   ,29   ,7   ,27   ,5   ,5);
INSERT INTO Timetable VALUES(150   ,3    ,1   ,30   ,7   ,26   ,6   ,1);
INSERT INTO Timetable VALUES(151   ,15   ,2   ,24   ,7   ,29   ,6   ,1);
INSERT INTO Timetable VALUES(152   ,1    ,1   ,30   ,7   ,26   ,6   ,2);
COMMIT;
