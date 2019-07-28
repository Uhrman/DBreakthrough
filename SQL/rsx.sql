-- --------------------------------------------------------
-- Хост:                         127.0.0.1
-- Версия сервера:               10.3.12-MariaDB
-- Операционная система:         Win64
-- HeidiSQL Версия:              9.4.0.5125
-- --------------------------------------------------------

-- Дамп структуры базы данных rsx
CREATE DATABASE IF NOT EXISTS `rsx` /*!40100 DEFAULT CHARACTER SET utf8 */;
USE `rsx`;

-- Экспортируемые данные не выделены.
-- Дамп структуры для таблица rsx.rsensors
CREATE TABLE IF NOT EXISTS `rsensors` (
  `t` decimal(12,3) DEFAULT NULL,
  `ax` decimal(12,6) DEFAULT 0.000000,
  `az` decimal(12,6) DEFAULT 0.000000,
  `ay` decimal(12,8) DEFAULT 0.00000000,
  `dummy1` decimal(12,8) DEFAULT 0.00000000,
  `dummy2` decimal(12,8) DEFAULT 0.00000000,
  `dummy3` decimal(12,8) DEFAULT 0.00000000,
  `dummy4` decimal(12,8) DEFAULT 0.00000000,
  `activity` int(11) DEFAULT NULL,
  `person` varchar(45) DEFAULT NULL,
  `gender` varchar(1) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
