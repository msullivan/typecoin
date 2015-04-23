drop table if exists txn;
create table txn (
	id integer primary key autoincrement not null,
	txn_body blob not null
);

drop table if exists resource;
create table resource (
	id integer primary key autoincrement not null,

	resource blob not null,
	debug_name string,

    spent int not null,

    -- only one of these should be non null
	real_txn_origin string, -- this is dum.
	batch_txn_origin int,
	idx int not null,

	original_origin_txn string, -- this is dum.
	original_origin_idx int not null,

	owner blob not null
);



-- Ought to have indexes!!
