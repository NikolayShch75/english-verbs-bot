# ============================================
# ENGLISH VERBS TELEGRAM BOT
# Автоматическая установка пакетов при необходимости
# ============================================

cat("🔧 Initializing bot...\n")

# Список необходимых пакетов
required_packages <- c("telegram.bot", "dplyr", "stringr")

# Функция для установки пакетов
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("📦 Installing package:", pkg, "\n")
      tryCatch({
        install.packages(pkg, repos = "https://cloud.r-project.org/", quiet = TRUE)
        cat("✅ Package", pkg, "installed\n")
      }, error = function(e) {
        cat("⚠️ Could not install", pkg, ":", e$message, "\n")
      })
    }
  }
}

# Устанавливаем недостающие пакеты
install_if_missing(required_packages)

# Загружаем пакеты
cat("📚 Loading packages...\n")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop("❌ Failed to load package: ", pkg)
  }
}
cat("✅ All packages loaded successfully\n")





# Полная база данных неправильных глаголов с исправлениями
verbs_data <- data.frame(
  infinitive = c(
    # Group A
    "be", "choose", "feed", "give", "keep", "mean", "see", "sleep", "teach", "feel",
    # Group B  
    "become", "come", "feel", "go", "know", "meet", "sell", "smell", "tell", "keep",
    # Group C
    "begin", "cost", "fight", "grow", "lay", "pay", "send", "speak", "think", "spend",
    # Group D
    "blow", "cut", "find", "hang", "lead", "lay", "set", "spend", "throw", "send",
    # Group E
    "break", "make", "fly", "have", "teach", "read", "shake", "spoil", "understand", "spread",
    # Group F
    "bring", "draw", "forecast", "hear", "leave", "ride", "shine", "spread", "wake", "spoil",
    # Group G
    "build", "drink", "forget", "hide", "let", "ring", "show", "stand", "wear", "rise",
    # Group H
    "burn", "drive", "forgive", "hit", "lie", "rise", "sing", "steal", "win", "forget",
    # Group L
    "buy", "eat", "freeze", "hold", "lose", "run", "sink", "swim", "write", "catch",
    # Group K
    "catch", "fall", "get", "hurt", "make", "speak", "sit", "take", "sink", "write"
  ),
  
  past_simple = c(
    # Group A
    "was/were", "chose", "fed", "gave", "kept", "meant", "saw", "slept", "taught", "felt",
    # Group B
    "became", "came", "felt", "went", "knew", "met", "sold", "smelt", "told", "kept", 
    # Group C
    "began", "cost", "fought", "grew", "laid", "paid", "sent", "spoke", "thought", "spent",
    # Group D
    "blew", "cut", "found", "hung", "led", "laid", "set", "spent", "threw", "sent",
    # Group E
    "broke", "made", "flew", "had", "taught", "read", "shook", "spoilt", "understood", "spread",
    # Group F
    "brought", "drew", "forecast", "heard", "left", "rode", "shone", "spread", "woke", "spoilt",
    # Group G
    "built", "drank", "forgot", "hid", "let", "rang", "showed", "stood", "wore", "rose",
    # Group H
    "burnt", "drove", "forgave", "hit", "lay", "rose", "sang", "stole", "won", "forgot",
    # Group L
    "bought", "ate", "froze", "held", "lost", "ran", "sank", "swam", "wrote", "caught",
    # Group K
    "caught", "fell", "got", "hurt", "made", "spoke", "sat", "took", "sank", "wrote"
  ),
  
  past_participle = c(
    # Group A
    "been", "chosen", "fed", "given", "kept", "meant", "seen", "slept", "taught", "felt",
    # Group B
    "become", "come", "felt", "gone", "known", "met", "sold", "smelt", "told", "kept",
    # Group C
    "begun", "cost", "fought", "grown", "laid", "paid", "sent", "spoken", "thought", "spent",
    # Group D
    "blown", "cut", "found", "hung", "led", "laid", "set", "spent", "thrown", "sent",
    # Group E
    "broken", "made", "flown", "had", "taught", "read", "shaken", "spoilt", "understood", "spread",
    # Group F
    "brought", "drawn", "forecast", "heard", "left", "ridden", "shone", "spread", "woken", "spoilt",
    # Group G
    "built", "drunk", "forgotten", "hidden", "let", "rung", "shown", "stood", "worn", "risen",
    # Group H
    "burnt", "driven", "forgiven", "hit", "lain", "risen", "sung", "stolen", "won", "forgotten",
    # Group L
    "bought", "eaten", "frozen", "held", "lost", "run", "sunk", "swum", "written", "caught",
    # Group K
    "caught", "fallen", "got/gotten", "hurt", "made", "spoken", "sat", "taken", "sunk", "written"
  ),
  
  translation = c(
    # Group A
    "быть", "выбирать", "кормить", "давать", "хранить, держать", "подразумевать, значить", "видеть", "спать", "учить", "чувствовать",
    # Group B
    "становиться", "приходить", "чувствовать", "идти", "знать", "встречать", "продавать", "пахнуть", "рассказывать", "хранить, держать",
    # Group C
    "начинать", "стоить", "сражаться", "расти", "класть", "платить", "посылать", "говорить", "думать", "тратить",
    # Group D
    "дуть", "резать", "находить", "вешать", "вести, руководить", "класть", "устанавливать", "тратить", "бросать", "посылать",
    # Group E
    "ломать", "делать", "летать", "иметь", "учить", "читать", "трясти", "портить", "понимать", "распространять",
    # Group F
    "приносить", "рисовать", "предсказывать", "слышать", "оставлять", "ездить верхом", "светить", "распространяться", "просыпаться", "портить",
    # Group G
    "строить", "пить", "забывать", "прятать(-ся)", "позволять", "звонить", "показывать", "стоять", "носить", "подниматься",
    # Group H
    "жечь", "водить", "прощать", "ударять", "лежать", "подниматься", "петь", "красть", "выигрывать", "забывать",
    # Group L
    "покупать", "кушать", "замерзать", "держать в руках", "терять", "бежать", "тонуть", "плавать", "писать", "ловить",
    # Group K
    "ловить", "падать", "получать", "причинять боль", "делать, производить", "говорить", "сидеть", "брать", "тонуть", "писать"
  ),
  stringsAsFactors = FALSE
)

# Удаляем дубликаты глаголов
verbs_data <- verbs_data[!duplicated(verbs_data$infinitive), ]

cat("✅ База данных создана! Всего уникальных глаголов:", nrow(verbs_data), "\n")

# Глобальные переменные
user_stats <- new.env()
current_tests <- new.env()
current_learning_verb <- new.env()

# Безопасные функции доступа
safe_get <- function(env, key) {
  if (exists(key, envir = env)) {
    return(get(key, envir = env))
  }
  return(NULL)
}

safe_set <- function(env, key, value) {
  assign(key, value, envir = env)
}

# Функция для создания клавиатуры (только 3 пункта)
create_main_keyboard <- function() {
  buttons <- list(
    list(InlineKeyboardButton("Учить глаголы", callback_data = "learn")),
    list(InlineKeyboardButton("Тест русский -> все формы", callback_data = "test_russian")),
    list(InlineKeyboardButton("Найти глагол", callback_data = "search"))
  )
  return(InlineKeyboardMarkup(inline_keyboard = buttons))
}

# Функция для безопасного редактирования сообщения
safe_edit_message <- function(bot, chat_id, message_id, text, reply_markup = NULL) {
  tryCatch({
    if (is.null(reply_markup)) {
      bot$editMessageText(chat_id = chat_id, message_id = message_id, text = text)
    } else {
      bot$editMessageText(chat_id = chat_id, message_id = message_id, text = text, reply_markup = reply_markup)
    }
  }, error = function(e) {
    if (!grepl("message is not modified", e$message)) {
      cat("Ошибка при редактировании сообщения:", e$message, "\n")
    }
  })
}

# Стартовое сообщение
start_handler <- function(bot, update) {
  user_id <- as.character(update$message$from$id)
  chat_id <- as.character(update$message$chat_id)
  
  if (is.null(safe_get(user_stats, user_id))) {
    safe_set(user_stats, user_id, list(
      correct_answers = 0,
      total_answers = 0,
      verbs_learned = 0
    ))
  }
  
  welcome_text <- paste(
    "Привет! Я помогу тебе выучить неправильные глаголы английского языка!",
    paste("В базе", nrow(verbs_data), "глаголов для изучения."),
    "Выбери действие:",
    sep = "\n"
  )
  
  bot$sendMessage(
    chat_id = chat_id,
    text = welcome_text,
    reply_markup = create_main_keyboard()
  )
}

# Режим обучения
learn_handler <- function(bot, update) {
  chat_id <- as.character(update$callback_query$message$chat$id)
  message_id <- update$callback_query$message$message_id
  
  verb_index <- sample(1:nrow(verbs_data), 1)
  safe_set(current_learning_verb, chat_id, verb_index)
  
  verb <- verbs_data[verb_index, ]
  
  learn_text <- paste(
    "Учим глагол:",
    paste("Английский:", verb$infinitive),
    paste("Русский:", verb$translation),
    "",
    "Формы:",
    paste("Past Simple:", verb$past_simple),
    paste("Past Participle:", verb$past_participle),
    "",
    "Примеры:",
    paste("I", verb$infinitive, "every day. (настоящее время)"),
    paste("I", ifelse(grepl("/", verb$past_simple), 
                      strsplit(verb$past_simple, "/")[[1]][1], 
                      verb$past_simple), "yesterday. (прошедшее время)"),
    paste("I have", verb$past_participle, "many times. (совершенное время)"),
    sep = "\n"
  )
  
  learn_buttons <- list(
    list(
      InlineKeyboardButton("Следующий глагол", callback_data = "learn_next"),
      InlineKeyboardButton("Выучил", callback_data = "learned")
    ),
    list(InlineKeyboardButton("Назад", callback_data = "back_main"))
  )
  learn_markup <- InlineKeyboardMarkup(inline_keyboard = learn_buttons)
  
  safe_edit_message(bot, chat_id, message_id, learn_text, learn_markup)
}

# Тест: Русский -> Все формы
test_russian_handler <- function(bot, update) {
  chat_id <- as.character(update$callback_query$message$chat$id)
  message_id <- update$callback_query$message$message_id
  
  verb_index <- sample(1:nrow(verbs_data), 1)
  verb <- verbs_data[verb_index, ]
  
  safe_set(current_tests, chat_id, list(
    type = "test_russian",
    correct_answers = list(
      infinitive = verb$infinitive,
      past_simple = verb$past_simple,
      past_participle = verb$past_participle
    ),
    verb_index = verb_index
  ))
  
  test_text <- paste(
    "ТЕСТ: Напиши все три формы глагола",
    paste("Русский:", verb$translation),
    "",
    "Формат ответа:",
    "infinitive past_simple past_participle",
    "",
    "Пример:",
    "go went gone",
    "",
    "Напиши ответ в чат:",
    sep = "\n"
  )
  
  safe_edit_message(bot, chat_id, message_id, test_text)
}

# Проверка ответа на тест
check_test_answer <- function(bot, update) {
  chat_id <- as.character(update$message$chat_id)
  user_id <- as.character(update$message$from$id)
  user_answer <- trimws(update$message$text)
  
  test <- safe_get(current_tests, chat_id)
  
  if (!is.null(test)) {
    verb <- verbs_data[test$verb_index, ]
    
    if (test$type == "test_russian") {
      # Разделяем ответ по пробелам
      user_answers <- strsplit(user_answer, "\\s+")[[1]] %>% 
        trimws()
      
      if (length(user_answers) == 3) {
        correct_infinitive <- tolower(verb$infinitive)
        correct_past_simple <- tolower(verb$past_simple) %>% strsplit("/") %>% .[[1]] %>% trimws()
        correct_past_participle <- tolower(verb$past_participle) %>% strsplit("/") %>% .[[1]] %>% trimws()
        
        infinitive_correct <- tolower(user_answers[1]) == correct_infinitive
        past_simple_correct <- any(tolower(user_answers[2]) %in% correct_past_simple)
        past_participle_correct <- any(tolower(user_answers[3]) %in% correct_past_participle)
        
        all_correct <- infinitive_correct && past_simple_correct && past_participle_correct
        
        user_stat <- safe_get(user_stats, user_id)
        user_stat$total_answers <- user_stat$total_answers + 1
        
        if (all_correct) {
          user_stat$correct_answers <- user_stat$correct_answers + 1
          safe_set(user_stats, user_id, user_stat)
          
          response_text <- paste(
            "✅ Отлично! Все формы правильные!",
            paste("Твой ответ:", user_answer),
            "",
            "Правильные формы:",
            paste("Infinitive:", verb$infinitive),
            paste("Past Simple:", verb$past_simple),
            paste("Past Participle:", verb$past_participle),
            sep = "\n"
          )
        } else {
          safe_set(user_stats, user_id, user_stat)
          
          errors <- c()
          if (!infinitive_correct) errors <- c(errors, paste("Infinitive: нужно", verb$infinitive))
          if (!past_simple_correct) errors <- c(errors, paste("Past Simple: нужно", verb$past_simple))
          if (!past_participle_correct) errors <- c(errors, paste("Past Participle: нужно", verb$past_participle))
          
          response_text <- paste(
            "❌ Есть ошибки:",
            paste("Твой ответ:", user_answer),
            "",
            "Ошибки в:",
            paste("-", errors, collapse = "\n"),
            "",
            "Правильные формы:",
            paste("Infinitive:", verb$infinitive),
            paste("Past Simple:", verb$past_simple),
            paste("Past Participle:", verb$past_participle),
            sep = "\n"
          )
        }
      } else {
        user_stat <- safe_get(user_stats, user_id)
        user_stat$total_answers <- user_stat$total_answers + 1
        safe_set(user_stats, user_id, user_stat)
        
        response_text <- paste(
          "❌ Неправильный формат ответа!",
          "Нужно ввести три формы через ПРОБЕЛ:",
          "infinitive past_simple past_participle",
          "",
          "Пример: go went gone",
          "",
          "Попробуй еще раз!",
          sep = "\n"
        )
      }
    }
    
    safe_set(current_tests, chat_id, NULL)
    
    bot$sendMessage(
      chat_id = chat_id,
      text = response_text,
      reply_markup = create_main_keyboard()
    )
    return(TRUE)
  }
  return(FALSE)
}

# Поиск глагола
search_handler <- function(bot, update) {
  chat_id <- as.character(update$callback_query$message$chat$id)
  message_id <- update$callback_query$message$message_id
  
  search_text <- "Введи глагол в инфинитиве (например: 'go' или 'be'):"
  safe_edit_message(bot, chat_id, message_id, search_text)
}

# Обработка поиска
handle_search <- function(bot, update) {
  if (check_test_answer(bot, update)) {
    return()
  }
  
  chat_id <- as.character(update$message$chat_id)
  search_term <- tolower(trimws(update$message$text))
  
  found_verb <- verbs_data %>% 
    filter(infinitive == search_term)
  
  if (nrow(found_verb) > 0) {
    verb <- found_verb[1, ]
    result_text <- paste(
      "Найден глагол:",
      paste("Infinitive:", verb$infinitive),
      paste("Translation:", verb$translation),
      paste("Past Simple:", verb$past_simple),
      paste("Past Participle:", verb$past_participle),
      sep = "\n"
    )
  } else {
    result_text <- paste(
      "Глагол не найден.",
      "Попробуй другой глагол или проверь написание.",
      sep = "\n"
    )
  }
  
  bot$sendMessage(
    chat_id = chat_id,
    text = result_text,
    reply_markup = create_main_keyboard()
  )
}

# Основной обработчик callback-ов
callback_handler <- function(bot, update) {
  data <- update$callback_query$data
  
  if (data == "learn" || data == "learn_next") {
    learn_handler(bot, update)
  } else if (data == "test_russian") {
    test_russian_handler(bot, update)
  } else if (data == "search") {
    search_handler(bot, update)
  } else if (data == "back_main") {
    chat_id <- as.character(update$callback_query$message$chat$id)
    message_id <- update$callback_query$message$message_id
    safe_edit_message(bot, chat_id, message_id, "Выбери действие:", create_main_keyboard())
  } else if (data == "learned") {
    chat_id <- as.character(update$callback_query$message$chat$id)
    user_id <- as.character(update$callback_query$from$id)
    
    user_stat <- safe_get(user_stats, user_id)
    if (!is.null(user_stat)) {
      user_stat$verbs_learned <- min(user_stat$verbs_learned + 1, nrow(verbs_data))
      safe_set(user_stats, user_id, user_stat)
    }
    
    bot$sendMessage(
      chat_id = chat_id,
      text = "Отлично! Глагол добавлен в изученные!",
      reply_markup = create_main_keyboard()
    )
  }
}

# Получаем токен из переменных окружения
BOT_TOKEN <- Sys.getenv("BOT_TOKEN")
#BOT_TOKEN <- "7906046158:AAGaRY-Dwqi3yc-e_7_J2rRaLN64dkLAfSU"
if (BOT_TOKEN == "") {
  stop("Токен бота не найден! Установите переменную BOT_TOKEN")
}

# Запуск бота с обработкой ошибок
cat("🤖 Запускаю бота для изучения английских глаголов...\n")
cat("Токен:", substr(BOT_TOKEN, 1, 10), "...\n")
cat("Глаголов в базе:", nrow(verbs_data), "\n")
cat("⏰ Время запуска:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

# Функция для перезапуска при ошибках
run_bot <- function() {
  tryCatch({
    updater <- Updater(BOT_TOKEN)
    
    updater <- updater +
      CommandHandler("start", start_handler) +
      CallbackQueryHandler(callback_handler) +
      MessageHandler(handle_search, MessageFilters$text)
    
    cat("✅ Бот успешно запущен и слушает сообщения...\n")
    updater$start_polling()
    
  }, error = function(e) {
    cat("❌ Ошибка:", e$message, "\n")
    cat("🔄 Перезапуск через 30 секунд...\n")
    Sys.sleep(30)
    run_bot()
  })
}

# Запускаем бота
run_bot()


