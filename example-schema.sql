DROP TABLE IF EXISTS Works;
DROP TABLE IF EXISTS Area;

CREATE TABLE "Works" (	
  Person TEXT,
  Project TEXT );
  
CREATE TABLE "Area" (	
  Project TEXT,
  Field TEXT );
  
DROP TABLE IF EXISTS Hasjob;
DROP TABLE IF EXISTS Teaches;
DROP TABLE IF EXISTS InField;
DROP TABLE IF EXISTS Get;
DROP TABLE IF EXISTS ForGrant;
  
CREATE TABLE "Hasjob" (	
  Person TEXT,
  Field TEXT );

CREATE TABLE "Teaches" (	
  Professor TEXT,
  Course TEXT );

CREATE TABLE "InField" (	
  Course TEXT,
  Field TEXT );
  
CREATE TABLE "Get" (	
  Researcher TEXT,
  Grant INTEGER );
  
CREATE TABLE "ForGrant" (	
  Grant INTEGER,
  Project TEXT );

INSERT INTO Hasjob(Person, Field) VALUES ('Romuald', 'CS');
INSERT INTO Hasjob(Person, Field) VALUES ('Romuald', 'Security');
INSERT INTO Hasjob(Person, Field) VALUES ('Romuald', 'DB');
INSERT INTO Hasjob(Person, Field) VALUES ('Manu', 'CS');
INSERT INTO Hasjob(Person, Field) VALUES ('Angela', 'DB');


INSERT INTO Teaches(Professor, Course) VALUES ('Romuald', 'TIW4');
INSERT INTO Teaches(Professor, Course) VALUES ('Romuald', 'DBDM');
INSERT INTO Teaches(Professor, Course) VALUES ('Angela', 'DBDM');
INSERT INTO Teaches(Professor, Course) VALUES ('Angela', 'BDW2');
INSERT INTO Teaches(Professor, Course) VALUES ('Manu', 'DBDM');
INSERT INTO Teaches(Professor, Course) VALUES ('Manu', 'TIW1');

INSERT INTO InField(Course, Field) VALUES ('TIW4', 'Security');
INSERT INTO InField(Course, Field) VALUES ('DBDM', 'DB');
INSERT INTO InField(Course, Field) VALUES ('BDW2', 'CS');

INSERT INTO Get(Researcher, Grant) VALUES ('Angela', 1);
INSERT INTO Get(Researcher, Grant) VALUES ('Angela', 2);
INSERT INTO Get(Researcher, Grant) VALUES ('Manu', 1);

INSERT INTO ForGrant(Grant, Project) VALUES (1, 'DataCert');
INSERT INTO ForGrant(Grant, Project) VALUES (2, 'MedClean');





