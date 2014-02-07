drop table if exists txn;
create table txn (
	id integer primary key autoincrement not null,
	txn_body blob not null
);

drop table if exists resource;
create table resource (
	id integer primary key autoincrement not null,
	resource blob not null,
	origin blob not null,
	owner blob not null
);

