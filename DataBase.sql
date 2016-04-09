CREATE DATABASE 'localhost:C:\myDb\DZ.fdb' USER 'SYSDBA' PASSWORD 'masterkey' DEFAULT CHARACTER SET UTF8;
CREATE GENERATOR GroupsIdGenerator;
CREATE GENERATOR LessonsIdGenerator;
CREATE GENERATOR TeachersIdGenerator;
CREATE GENERATOR ClassRoomsIdGenerator;
CREATE GENERATOR LessonsTimesIdGenerator;
CREATE GENERATOR WeekDaysIdGenerator;
CREATE GENERATOR TimeTableIdGenerator;
CREATE GENERATOR LessonsTypesIdGenerator;

CREATE TABLE Groups(
	id INTEGER NOT NULL PRIMARY KEY,
	name VARCHAR(10)
);

CREATE TABLE Lessons(
	id INTEGER NOT NULL PRIMARY KEY,
	name VARCHAR(100)
);

CREATE TABLE Teachers(
	id INTEGER NOT NULL PRIMARY KEY,
	last_name VARCHAR(20),
	first_name VARCHAR(20), 
   middle_name VARCHAR(20)
);

CREATE TABLE Class_Rooms(
	id INTEGER NOT NULL PRIMARY KEY,
	name VARCHAR(10)
);

CREATE TABLE Lessons_Times(
	id INTEGER NOT NULL PRIMARY KEY,
	begin_ TIME,
	end_ TIME
);

CREATE TABLE Week_Days(
	id INTEGER NOT NULL PRIMARY KEY,
	name VARCHAR(20)
);

CREATE TABLE Lessons_Types(
	id INTEGER NOT NULL PRIMARY KEY,
	name VARCHAR(50)
);

CREATE TABLE Time_Table(
	id INTEGER NOT NULL PRIMARY KEY,
	lesson_id INTEGER REFERENCES Lessons(id),
	lesson_type_id INTEGER REFERENCES Lessons_Types(id),
	teacher_id INTEGER REFERENCES Teachers(id),
	group_id INTEGER REFERENCES Groups(id),
	class_room_id INTEGER REFERENCES Class_Rooms(id),
	week_day_id INTEGER REFERENCES Week_Days(id),
	lesson_time_id INTEGER REFERENCES Lessons_Times(id)
);

SET TERM ^ ;

CREATE TRIGGER GroupsIdTrigger FOR Groups ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR GroupsIdGenerator;
END^

CREATE TRIGGER TeachersIdTrigger FOR Teachers ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR TeachersIdGenerator;
END^

CREATE TRIGGER LessonsIdTrigger FOR Lessons ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR LessonsIdGenerator;
END^

CREATE TRIGGER LessonsTimesIdTrigger FOR Lessons_Times ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR LessonsTimesIdGenerator;
END^

CREATE TRIGGER WeekDaysIdTrigger FOR Week_Days ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR WeekDaysIdGenerator;
END^

CREATE TRIGGER ClassRoomsIdTrigger FOR Class_Rooms ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR ClassRoomsIdGenerator;
END^

CREATE TRIGGER LessonsTypesIdTrigger FOR Lessons_Types ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR LessonsTypesIdGenerator;
END^

CREATE TRIGGER TimeTableIdTrigger FOR Time_Table ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR TimeTableIdGenerator;
END^

SET TERM ; ^

COMMIT;
INSERT INTO Groups VALUES(1, 'Б8103а1');
INSERT INTO Groups VALUES(2, 'Б8103а2');
INSERT INTO Groups VALUES(3, 'Б8103б1');
INSERT INTO Groups VALUES(4, 'Б8103б2');

INSERT INTO Groups VALUES(5, 'Б8203а');

INSERT INTO Lessons VALUES(1, 'Математический анализ');
INSERT INTO Lessons VALUES(2, 'Дискретая математика и математическая логика');
INSERT INTO Lessons VALUES(3, 'Алгебра и геометрия');
INSERT INTO Lessons VALUES(4, 'Иностранный язык');
INSERT INTO Lessons VALUES(5, 'Практикум на ЭВМ');
INSERT INTO Lessons VALUES(6, 'Языки и методы программирования');
INSERT INTO Lessons VALUES(7, 'Базы данных');
INSERT INTO Lessons VALUES(8, 'Архитектура компьютеров');
INSERT INTO Lessons VALUES(9, 'Элективные курсы по физической культуре');

INSERT INTO Lessons VALUES(10, 'Теория вероятностей и математическая статистика');
INSERT INTO Lessons VALUES(11, 'Обыкновенные дифференциальные уравнения');
INSERT INTO Lessons VALUES(12, 'Теоретическая механика и физика');
INSERT INTO Lessons VALUES(13, 'Экономика');
INSERT INTO Lessons VALUES(14, 'Численные методы');
INSERT INTO Lessons VALUES(15, 'Технология программирования');
INSERT INTO Lessons VALUES(16, 'Русский язык и культура речи');
INSERT INTO Lessons VALUES(17, 'Численные методы');

INSERT INTO Teachers VALUES(1, 'Кленин', 'Александр', 'Сергеевич');
INSERT INTO Teachers VALUES(2, 'Клевчихин', 'Юрий', 'Александрович');
INSERT INTO Teachers VALUES(3, 'Прилепкина', 'Елена', 'Гумаровна');
INSERT INTO Teachers VALUES(4, 'Спорышев', 'Максим', 'Сергеевич');
INSERT INTO Teachers VALUES(5, 'Машенцев', 'Владимир', 'Юрьевич');
INSERT INTO Teachers VALUES(6, 'Пак', 'Геннадий', 'Константинович');
INSERT INTO Teachers VALUES(7, 'Трикашная', 'Наталья', 'Вячеславовна');
INSERT INTO Teachers VALUES(8, 'Кириленко', 'Лариса', 'Михайловна');
INSERT INTO Teachers VALUES(9, 'Баранов', 'Андрей', 'Александрович');
INSERT INTO Teachers VALUES(10, 'Алексанин', 'Григорий', 'Анатольевич');
INSERT INTO Teachers VALUES(11, 'Веремеева', 'Ирина', 'Феликсовна');
INSERT INTO Teachers VALUES(12, 'Абстрактный', 'Учитель', 'Физкультуры');

INSERT INTO Teachers VALUES(13, 'Лиховидов', 'Виктор', 'Николаевич');
INSERT INTO Teachers VALUES(14, 'Шепелева', 'Риорита', 'Петровна');
INSERT INTO Teachers VALUES(15, 'Достовалов', 'Валерий', 'Николаевич');
INSERT INTO Teachers VALUES(16, 'Величко', 'Андрей', 'Сергеевич');
INSERT INTO Teachers VALUES(17, 'Пак', 'Татьяна', 'Владимировна');

INSERT INTO Lessons_Times VALUES(1, '08:30:00.0000', '10:00:00.0000');
INSERT INTO Lessons_Times VALUES(2, '10:10:00.0000', '11:40:00.0000');
INSERT INTO Lessons_Times VALUES(3, '11:50:00.0000', '13:20:00.0000');
INSERT INTO Lessons_Times VALUES(4, '13:30:00.0000', '15:00:00.0000');
INSERT INTO Lessons_Times VALUES(5, '15:10:00.0000', '16:40:00.0000');
INSERT INTO Lessons_Times VALUES(6, '16:50:00.0000', '18:20:00.0000');
INSERT INTO Lessons_Times VALUES(7, '18:30:00.0000', '20:00:00.0000');
INSERT INTO Lessons_Times VALUES(8, '20:10:00.0000', '21:40:00.0000');

INSERT INTO Week_Days VALUES(1, 'Понедельник');
INSERT INTO Week_Days VALUES(2, 'Вторник');
INSERT INTO Week_Days VALUES(3, 'Среда');
INSERT INTO Week_Days VALUES(4, 'Четверг');
INSERT INTO Week_Days VALUES(5, 'Пятница');
INSERT INTO Week_Days VALUES(6, 'Суббота');
INSERT INTO Week_Days VALUES(7, 'Воскресенье');

INSERT INTO Class_Rooms VALUES(1, 'D733');
INSERT INTO Class_Rooms VALUES(2, 'D733a');
INSERT INTO Class_Rooms VALUES(3, 'D654/752');
INSERT INTO Class_Rooms VALUES(4, 'D412/542');
INSERT INTO Class_Rooms VALUES(5, 'S');
INSERT INTO Class_Rooms VALUES(6, 'D547');
INSERT INTO Class_Rooms VALUES(7, 'D741');
INSERT INTO Class_Rooms VALUES(8, 'D743');
INSERT INTO Class_Rooms VALUES(9, 'D549a');
INSERT INTO Class_Rooms VALUES(10, 'D746');

INSERT INTO Class_Rooms VALUES(11, 'D738'); 
INSERT INTO Class_Rooms VALUES(12, 'D818');
INSERT INTO Class_Rooms VALUES(13, 'D945');
INSERT INTO Class_Rooms VALUES(14, 'D746');
INSERT INTO Class_Rooms VALUES(15, 'D746');
INSERT INTO Class_Rooms VALUES(16, 'D548');

INSERT INTO Lessons_Types VALUES(1, 'Практическое занятие');
INSERT INTO Lessons_Types VALUES(2, 'Лекция');
INSERT INTO Lessons_Types VALUES(3, 'Лабораторное занятие');


INSERT INTO Time_Table VALUES(1, 1, 2, 3, 1, 3, 1, 2);
INSERT INTO Time_Table VALUES(2, 1, 2, 3, 2, 3, 1, 2);
INSERT INTO Time_Table VALUES(3, 2, 2, 6, 1, 4, 1, 3);
INSERT INTO Time_Table VALUES(4, 2, 2, 6, 2, 4, 1, 3);
INSERT INTO Time_Table VALUES(5, 3, 2, 6, 1, 4, 1, 4);
INSERT INTO Time_Table VALUES(6, 3, 2, 6, 2, 4, 1, 4);
INSERT INTO Time_Table VALUES(7, 9, 1, 12, 1, 5, 1, 5);
INSERT INTO Time_Table VALUES(8, 9, 1, 12, 2, 5, 1, 5);

INSERT INTO Time_Table VALUES(9, 10, 2, 13, 5, 3, 2, 2);
INSERT INTO Time_Table VALUES(10, 11, 2, 14, 5, 11, 2, 3);
INSERT INTO Time_Table VALUES(11, 10, 1, 13, 5, 16, 2, 4);
INSERT INTO Time_Table VALUES(12, 9, 1, 12, 5, 5, 2, 5);

COMMIT;