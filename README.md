---
title: "Sanity Chronicles 🧠📆"
description: "A private (or demo) daily mood tracking app built with Shiny + Supabase"
license: "MIT"
---

# Sanity Chronicles 🧠📆

A mobile-friendly Shiny app for daily mood & stress tracking with calendar and trend views.  
Supports **owner (private)** and **guest (public demo)** modes.

---

## Features

- One entry per day (time-restricted)
- Mood buttons + stress slider
- Optional note
- Entries **only** allowed between **18:00–23:59** (local time)
- Year calendar + trend plot
- CSV / JSON export
- Mobile-optimized UI
- Supabase (Postgres) backend

---

## Tech Stack

- **R / Shiny**
- **Supabase Postgres** (via pooler)
- **Posit Connect Cloud**
- Client-side JS (guest ID, timezone, viewport)

---

## Repository

```
.
├── app.R
├── README.md
└── .secret_owner_password.r  # local only, not committed
```

---

## Quick Start

> ⚠️ **Important notes before you begin**
>
> - This app can be run **entirely locally** (no Supabase, no deployment)
> - Supabase is **optional**, but required if you want persistent storage across sessions or deployments
> - Parts of the code for alternative/local storage are present but **commented out**
> - If you add or change questions, you **must update the database schema** accordingly (see note below)

---

### 1) Fork & clone

```bash
git clone https://github.com/YOURNAME/sanity-chronicles.git
cd sanity-chronicles
```

---

### 2) Supabase setup

Supabase is only required if you want:
- persistent storage
- multi-session usage
- hosted deployment (e.g. Posit Connect Cloud)

If you only want to run the app locally **without persistence**, you can skip this step.

Create a Supabase project and run:

```sql
create table public.mood_logs (
  id bigserial primary key,
  user_id text not null,
  mode text not null,
  entry_date date not null,
  entry_time time not null,
  mood integer not null,
  label text not null,
  stress integer,
  note text
);
```

> ⚠️ If you add additional questions or inputs to the app,  
> you **must add corresponding columns** to this table and update:
> - the `INSERT` query
> - the `SELECT` query
> - any export logic (CSV / JSON)

---

### 3) Environment variables (Supabase + owner login)

These variables are **only required** if:
- you use Supabase **or**
- you deploy the app (e.g. Posit Connect Cloud)

Set them in your environment (example: **Posit Connect Cloud → Settings → Variables**):

```bash
SUPABASE_HOST=aws-0-<region>.pooler.supabase.com
SUPABASE_PORT=5432
SUPABASE_DB=postgres
SUPABASE_USER=postgres.<project-ref>
SUPABASE_PASSWORD=your_db_password
OWNER_PASSWORD=your_owner_password
```

Notes:
- Variable names must match **exactly**
- Do **not** wrap values in quotes
- For local development, these can also be set in `.Renviron`

> 💡 If you want to use Supabase **locally**, you still need to set these variables locally  
> (they are not pulled from the repository for security reasons)

---

### 4) Run locally

To run the app locally:

```r
shiny::runApp()
```

- If Supabase variables are **not** set, the app will run in local/demo mode
- If Supabase variables **are** set, data will be written to Supabase

---

### 5) Deploy (Posit Connect Cloud)

1. Set all required **environment variables in Posit Connect Cloud first**
2. Then deploy / publish the repository

This ensures:
- the app can connect to Supabase on first startup
- deployment does not fail due to missing credentials

Deployment steps:
- Push the repository to GitHub
- Create a new Shiny app in Posit Connect Cloud (from existing repository on Github)
- Add environment variables
- Deploy

> ℹ️ Supabase credentials should **never** be committed to the repository.

---

### Summary

| Use case | Supabase needed | Env vars needed |
|--------|----------------|-----------------|
| Local demo / testing | ❌ No | ❌ No |
| Local + persistent data | ✅ Yes | ✅ Yes |
| Hosted deployment | ✅ Yes | ✅ Yes |

---

## Notes

- Guest sessions use browser `localStorage`
- Owner data uses `user_id = "owner"`
- iOS Home Screen works (no push notifications)

---

## License

MIT
