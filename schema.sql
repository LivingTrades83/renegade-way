CREATE SCHEMA ibkr;

CREATE TYPE ibkr.action AS ENUM
    ('BUY',
     'SELL',
     'SSHORT');

CREATE TYPE ibkr.condition_comparator AS ENUM
    ('LESS THAN',
     'GREATER THAN');

CREATE TYPE ibkr.condition_operator AS ENUM
    ('AND',
     'OR');

CREATE TYPE ibkr.condition_trigger_method AS ENUM
    ('DEFAULT',
     'DOUBLE BID/ASK',
     'LAST',
     'DOUBLE LAST',
     'BID/ASK',
     'LAST OF BID/ASK',
     'MID POINT');

CREATE TYPE ibkr.condition_type AS ENUM
    ('PRICE',
     'TIME',
     'MARGIN',
     'EXECUTION',
     'VOLUME',
     'PERCENT CHANGE');

CREATE TYPE ibkr.open_close AS ENUM
    ('OPEN',
     'CLOSE',
     'SAME');

CREATE TYPE ibkr.order_strategy AS ENUM
    ('LONG CALL',
     'LONG PUT',
     'BULL CALL VERTICAL SPREAD',
     'BEAR CALL VERTICAL SPREAD',
     'BULL PUT VERTICAL SPREAD',
     'BEAR PUT VERTICAL SPREAD',
     'LONG STRADDLE',
     'LONG STRANGLE',
     'CALL RATIO SPREAD',
     'PUT RATIO SPREAD',
     'CALL HORIZONTAL SPREAD',
     'PUT HORIZONTAL SPREAD',
     'CALL DIAGONAL SPREAD',
     'PUT DIAGONAL SPREAD',
     'CALL BUTTERFLY',
     'PUT BUTTERFLY',
     'CALL CONDOR',
     'PUT CONDOR');

CREATE TYPE ibkr.pattern AS ENUM
    ('BULL PULLBACK',
     'BEAR RALLY',
     'HIGH BASE',
     'LOW BASE',
     'ASCENDING TRIANGLE',
     'DESCENDING TRIANGLE',
     'RANGE RALLY',
     'RANGE PULLBACK',
     'INCREASING RANK',
     'DECREASING RANK',
     'INCREASING VOL',
     'DECREASING VOL');

CREATE TYPE ibkr."right" AS ENUM
    ('CALL',
     'PUT');

CREATE TYPE ibkr.security_type AS ENUM
    ('STK',
     'OPT',
     'FUT',
     'CASH',
     'BOND',
     'CFD',
     'FOP',
     'WAR',
     'IOPT',
     'FWD',
     'BAG',
     'IND',
     'BILL',
     'FUND',
     'FIXED',
     'SLB',
     'NEWS',
     'CMDTY',
     'BSK',
     'ICU',
     'ICS');

CREATE TYPE ibkr.time_in_force AS ENUM
    ('DAY',
     'GTC',
     'OPG',
     'IOC',
     'GTD',
     'GTT',
     'AUC',
     'FOK',
     'GTX',
     'DTC');

CREATE TABLE ibkr.closing_trade_association
(
    opening_order_id int4 NOT NULL,
    execution_id text NOT NULL,
    CONSTRAINT closing_trade_association_execution_id_key UNIQUE (execution_id)
);

CREATE TABLE ibkr.contract
(
    symbol text,
    security_type ibkr.security_type,
    expiry date,
    strike numeric,
    "right" ibkr."right",
    exchange text,
    currency text,
    local_symbol text,
    market_name text,
    trading_class text,
    contract_id bigint NOT NULL,
    minimum_tick_increment numeric,
    multiplier numeric,
    price_magnifier integer,
    underlying_contract_id bigint,
    long_name text,
    primary_exchange text,
    contract_month text,
    industry text,
    category text,
    subcategory text,
    time_zone text,
    ev_rule text,
    ev_multiplier text,
    CONSTRAINT contract_pkey PRIMARY KEY (contract_id)
);

CREATE TABLE ibkr.execution
(
    order_id integer NOT NULL,
    contract_id bigint,
    execution_id text,
    "timestamp" timestamp with time zone,
    account text,
    executing_exchange text,
    side text,
    shares numeric,
    price numeric,
    perm_id integer,
    client_id integer,
    liquidation integer,
    cumulative_quantity integer,
    average_price numeric,
    order_reference text,
    model_code text,
    CONSTRAINT execution_execution_id_key UNIQUE (execution_id)
);

CREATE INDEX ON ibkr.execution ("timestamp");

CREATE TABLE ibkr.commission_report
(
    execution_id text,
    commission numeric,
    currency text,
    realized_pnl numeric,
    yield numeric,
    yield_redemption_date integer,
    CONSTRAINT commission_report_execution_id_key UNIQUE (execution_id),
    CONSTRAINT commission_report_execution_id_fkey FOREIGN KEY (execution_id)
        REFERENCES ibkr.execution (execution_id) MATCH SIMPLE
	ON UPDATE NO ACTION
	ON DELETE NO ACTION
);

CREATE TABLE ibkr."order"
(
    order_id integer NOT NULL,
    contract_id bigint,
    action ibkr.action,
    total_quantity numeric,
    order_type text,
    limit_price numeric,
    aux_price numeric,
    time_in_force ibkr.time_in_force,
    account text NOT NULL,
    open_close ibkr.open_close,
    order_ref text,
    client_id integer,
    perm_id integer,
    "timestamp" timestamp with time zone,
    CONSTRAINT order_pkey PRIMARY KEY (account, order_id)
);

CREATE TABLE ibkr.order_condition
(
    account text NOT NULL,
    order_id integer NOT NULL,
    contract_id bigint,
    type ibkr.condition_type NOT NULL,
    operator ibkr.condition_operator,
    comparator ibkr.condition_comparator,
    value text,
    exchange text,
    trigger_method ibkr.condition_trigger_method,
    CONSTRAINT order_condition_pkey PRIMARY KEY (account, order_id, type, comparator),
    CONSTRAINT order_condition_order_id_fkey FOREIGN KEY (account, order_id)
        REFERENCES ibkr."order" (account, order_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
);

CREATE TABLE ibkr.order_leg
(
    account text NOT NULL,
    order_id integer NOT NULL,
    contract_id bigint NOT NULL,
    ratio integer,
    action ibkr.action,
    exchange text,
    open_close ibkr.open_close,
    short_sale_slot smallint,
    designated_location text,
    exempt_code integer,
    CONSTRAINT order_leg_pkey PRIMARY KEY (account, order_id, contract_id),
    CONSTRAINT order_leg_order_id_fkey FOREIGN KEY (account, order_id)
        REFERENCES ibkr."order" (account, order_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
);

CREATE TABLE ibkr.order_note
(
    account text NOT NULL,
    order_id integer NOT NULL,
    order_strategy ibkr.order_strategy,
    underlying_entry_price numeric,
    underlying_stop_price numeric,
    underlying_target_price numeric,
    end_date date,
    pattern ibkr.pattern,
    CONSTRAINT order_note_order_id_key UNIQUE (account, order_id)
);

CREATE OR REPLACE VIEW ibkr."position"
AS SELECT execution.order_id,
    execution.contract_id,
    contract.symbol,
    contract.security_type,
    contract.expiry,
    contract.strike,
    contract."right",
    execution.account,
    sum(
        CASE execution.side
            WHEN 'BOT'::text THEN execution.shares
            WHEN 'SLD'::text THEN execution.shares * '-1'::integer::numeric
            ELSE NULL::numeric
        END) AS signed_shares,
    min(execution."timestamp") AS entry_timestamp
   FROM ibkr.execution
     JOIN ibkr.contract ON execution.contract_id = contract.contract_id
  WHERE execution.order_id > 0
  GROUP BY
    execution.order_id,
    execution.contract_id,
    contract.symbol,
    contract.security_type,
    contract.expiry,
    contract.strike,
    contract."right",
    execution.account;

CREATE SCHEMA renegade;

CREATE TABLE renegade.price_analysis (
    "date" date NOT NULL,
    market_act_symbol text NULL,
    market_rating int2 NULL,
    sector_act_symbol text NULL,
    sector_vs_market numeric NULL,
    sector_rating int2 NULL,
    industry_act_symbol text NULL,
    industry_rating int2 NULL,
    stock_act_symbol text NOT NULL,
    stock_vs_sector numeric NULL,
    dividend_date date NULL,
    earnings_date date NULL,
    option_spread numeric NULL,
    zacks_rank int2 NULL,
    patterns text NULL,
    CONSTRAINT price_analysis_pkey PRIMARY KEY (date, stock_act_symbol)
);

CREATE TABLE renegade.condor_analysis (
    "date" date NOT NULL,
    market_act_symbol text NULL,
    market_rating numeric NULL,
    market_risk_reward numeric NULL,
    sector_act_symbol text NULL,
    sector_rating numeric NULL,
    sector_risk_reward numeric NULL,
    industry_act_symbol text NULL,
    industry_rating numeric NULL,
    industry_risk_reward numeric NULL,
    stock_act_symbol text NOT NULL,
    stock_rating numeric NULL,
    stock_risk_reward numeric NULL,
    earnings_date date NULL,
    option_spread numeric NULL,
    CONSTRAINT condor_analysis_pkey PRIMARY KEY (date, stock_act_symbol)
);
