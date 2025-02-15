import configparser
import os
import re
import logging
from pathlib import Path
import xml.etree.ElementTree as ET
import xml.dom.minidom
from typing import Dict, List, Set, Optional, Tuple

try:
    from colorama import init, Fore, Style
except ImportError:
    # Если colorama не установлена, создаём заглушки
    def init():
        pass  # Пустая функция

    # Заглушки для цветов
    class DummyColor:
        def __getattr__(self, name):
            return ""

    Fore = DummyColor()
    Style = DummyColor()


class DelphiProjectProcessor:
    # Инициализация colorama
    init()
    # Standard Delphi units that should not trigger dependency warnings
    STANDARD_DELPHI_UNITS = {
        "System",
        "SysInit",
        "FileCtrl",
        "Math",
        "TypInfo",
        "Classes",
        "Windows",
        "Messages",
        "SysUtils",
        "Variants",
        "Classes",
        "Graphics",
        "Controls",
        "Forms",
        "Dialogs",
        "StdCtrls",
        "ExtCtrls",
        "ComCtrls",
        "ActnList",
        "ImgList",
        "Menus",
        "WinTypes",
        "WinProcs",
        "DbiTypes",
        "DbiProcs",
        "DbiErrs",
        "BDE",
    }

    def __init__(self, project_path: str):
        self.project_path = Path(project_path)
        self.project_dir = self.project_path.parent
        self.dof_path = self.project_path.with_suffix(".dof")
        config = configparser.ConfigParser()
        dependencies: Set[Path] = set()
        missing_dependencies: Set[str] = set()
        conditional_defines: Set[str] = set()
        processed_files: Set[Path] = set()
        dependency_graph: Dict[str, Set[str]] = {}
        self.dependency_graph = dependency_graph
        self.config = config
        self.dependencies = dependencies
        self.missing_files = missing_dependencies
        self.conditional_defines = conditional_defines
        self.processed_files = processed_files
        self.unprocessed_settings: Dict[str, Set[str]] = {}
        self.logger = logging.getLogger(__name__)

    def validate_path(self, path: Path) -> bool:
        """Проверяет существование и доступность пути"""
        try:
            return path.exists() and os.access(path, os.R_OK)
        except (OSError, PermissionError) as e:
            self.logger.error(f"Error validating path {path}: {e}")
            return False

    def convert_to_absolute_path(self, path: str) -> Path:
        """Конвертирует относительный путь в абсолютный"""
        if not path:
            return self.project_dir
        path_obj = Path(path)
        if path_obj.is_absolute():
            return path_obj
        return (self.project_dir / path_obj).resolve()

    def read_dof(self) -> None:
        """Читает и парсит DOF файл"""
        if not self.validate_path(self.dof_path):
            raise FileNotFoundError(
                f"DOF file not found or not accessible: {self.dof_path}"
            )

        self.logger.info(f"Reading DOF file: {self.dof_path}")
        self.config.read(self.dof_path)

        # Отслеживание необработанных секций
        known_sections = {
            "Compiler",
            "Directories",
            "Parameters",
            "Version Info",
            "Linker",
        }
        self.unprocessed_settings = {
            section: set(self.config[section].keys())
            for section in self.config.sections()
            if section not in known_sections
        }

        # Чтение условных определений
        if "Directories" in self.config:
            defines = self.config["Directories"].get("Conditionals", "")
            self.conditional_defines = {
                d.strip() for d in defines.split(";") if d.strip()
            }
            self.logger.debug(f"Found conditional defines: {self.conditional_defines}")

    def convert_to_dproj(self) -> ET.Element:
        """Конвертирует настройки DOF в формат DPROJ"""
        project = ET.Element(
            "Project", xmlns="http://schemas.microsoft.com/developer/msbuild/2003"
        )

        # Основные свойства проекта
        prop_group = ET.SubElement(project, "PropertyGroup")
        ET.SubElement(prop_group, "ProjectGuid").text = "{" + os.urandom(16).hex() + "}"
        ET.SubElement(prop_group, "MainSource").text = self.project_path.name
        ET.SubElement(prop_group, "Base").text = "True"
        ET.SubElement(prop_group, "Config").text = "Debug"

        # Конвертация настроек компилятора
        if "Compiler" in self.config:
            compiler_group = ET.SubElement(
                project,
                "PropertyGroup",
                Condition="'$(Config)'=='Base' or '$(Base)'!=''",
            )
            for key, value in self.config["Compiler"].items():
                ET.SubElement(compiler_group, key).text = value

        # Конвертация путей
        if "Directories" in self.config:
            dir_group = ET.SubElement(
                project,
                "PropertyGroup",
                Condition="'$(Config)'=='Base' or '$(Base)'!=''",
            )
            for key, value in self.config["Directories"].items():
                ET.SubElement(dir_group, key).text = value

        return project

    def analyze_dependencies(self) -> Set[Path]:
        """Анализирует зависимости проекта"""
        self.dependencies.clear()
        self._analyze_dpr_file()
        return self.dependencies

    def _analyze_dpr_file(self) -> None:
        """Анализирует DPR файл и находит зависимости"""
        with open(self.project_path, "r", encoding="utf-8", errors="ignore") as f:
            content = f.read()

        # Поиск uses-секций
        uses_pattern = r"uses\s+(.*?);"
        for uses_match in re.finditer(uses_pattern, content, re.DOTALL | re.IGNORECASE):
            units = uses_match.group(1).replace("\n", " ").split(",")
            clean_units = []
            for unit in units:
                unit = unit.strip()
                if unit:
                    self._add_dependency(unit)
                    clean_units.append(unit)
            # Добавляем зависимости в граф
            self._build_dependency_graph(str(self.project_path), clean_units)

    def _get_module_name(self, path: str) -> str:
        """Get the module name from a file path (case-insensitive)."""
        name = Path(
            path
        ).stem.lower()  # Используем stem вместо name для удаления расширения
        return name

    def _build_dependency_graph(self, source: str, dependencies: List[str]) -> None:
        """Build the dependency graph for a source module and its dependencies."""
        source_module = Path(source).stem
        self.logger.debug(f"Building graph for: {source_module}")
        if source_module in self.dependency_graph and dependencies:
            self.logger.debug(f"Updating dependencies for: {source_module}")

        if source_module not in self.dependency_graph:
            self.dependency_graph[source_module] = set()

        for dep in dependencies:
            # Извлекаем чистое имя модуля из зависимости
            dep_clean = re.sub(
                r"\s+in\s+'.*?'\s*(\{.*?\})?", "", dep, flags=re.IGNORECASE
            )
            dep_module = Path(dep_clean).stem

            if dep_module.lower() not in {
                u.lower() for u in self.STANDARD_DELPHI_UNITS
            }:
                self.dependency_graph[source_module].add(dep_module)
                self.logger.debug(f"Added dependency: {source_module} -> {dep_module}")

    def _detect_cycles(self) -> List[List[str]]:
        """Detect cycles in the dependency graph using DFS."""
        visited = set()
        rec_stack = set()
        cycles = []

        def dfs(node: str, path: List[str]) -> None:
            if node in rec_stack:
                cycle_start = path.index(node)
                cycle = path[cycle_start:] + [node]
                cycles.append(cycle)
                return

            if node in visited:
                return

            visited.add(node)
            rec_stack.add(node)
            path.append(node)

            for neighbor in self.dependency_graph.get(node, []):
                dfs(neighbor, path)

            path.pop()
            rec_stack.remove(node)

        for node in self.dependency_graph:
            if node not in visited:
                dfs(node, [])

        return cycles

    def _analyze_pas_file(self, path: Path) -> None:
        """Анализирует PAS файл на предмет зависимостей и секций uses"""
        if path in self.processed_files:
            self.logger.debug(f"Skipping already processed file: {path}")
            return

        self.processed_files.add(path)
        self.logger.info(f"Analyzing PAS file: {path}")

        try:
            with open(path, "r", encoding="utf-8", errors="ignore") as f:
                content = f.read()
        except Exception as e:
            self.logger.error(f"Не удалось прочитать файл {path}: {e}")
            return

        self.logger.debug("PAS file content loaded")

        # Поиск включаемых файлов
        include_pattern = r"\{$I\s+([^}]+)\}"
        for inc_match in re.finditer(include_pattern, content, re.IGNORECASE):
            inc_file = inc_match.group(1).strip()
            self.logger.debug(f"Found include file: {inc_file}")
            self._add_dependency(inc_file)

        # Анализ секций uses
        self._analyze_uses_dependencies(content, str(path))

        # Анализ условной компиляции
        ifdef_pattern = r"\{$IFDEF\s+([^}]+)\}(.*?)\{$ENDIF\}"
        for ifdef_match in re.finditer(ifdef_pattern, content, re.DOTALL):
            condition = ifdef_match.group(1).strip()
            if condition in self.conditional_defines:
                block_content = ifdef_match.group(2)
                self.logger.debug(
                    f"Analyzing conditional block for condition: {condition}"
                )
                self._analyze_conditional_block(block_content)

    def _resolve_module_path(self, module_name: str) -> Optional[Path]:
        """
        Resolves a module name to its actual file path.
        Args:
            module_name: The name of the module (e.g., 'Utils' or 'Utils.pas')
        Returns:
            Path object if found, None if not found
        """
        # First try direct module name
        possible_paths = [
            self.project_dir / f"{module_name}.pas",
            self.project_dir / module_name,
        ]

        # Add search paths from Directories section if available
        if "Directories" in self.config:
            search_paths = self.config["Directories"].get("SearchPath", "").split(";")
            for search_path in search_paths:
                if search_path.strip():
                    search_dir = self.convert_to_absolute_path(search_path.strip())
                    possible_paths.extend(
                        [search_dir / f"{module_name}.pas", search_dir / module_name]
                    )

        for path in possible_paths:
            if self.validate_path(path):
                return path
        return None

    def _add_dependency(self, name: str) -> None:
        """Добавляет зависимость в список"""
        self.logger.debug(f"Processing dependency: {name}")

        # Обработка формата "UnitName in 'FilePath' {FormClass}"
        form_pattern = r"""
            ^\s* 
            ([\w\.]+)                # Имя модуля
            \s+in\s+'(.*?\.pas)'    # Путь к файлу с расширением .pas
            (?:\s*\{.*?\})?         # Опциональный комментарий в фигурных скобках
            \s*$
        """
        form_match = re.match(form_pattern, name, re.X | re.IGNORECASE)

        if form_match:
            module_name = form_match.group(1).strip()
            file_path = form_match.group(2).strip()
            self.logger.debug(
                f"Found explicit path dependency: {module_name} -> {file_path}"
            )

            # Добавляем оба варианта зависимости
            self._process_module(module_name)
            self._process_file_path(file_path)
            return

        # Обработка простого имени модуля
        if re.match(r"^[a-zA-Z_][\w\.]*$", name):
            self._process_module(name)
        else:
            self.logger.warning(f"Invalid dependency format: {name}")

    def _process_module(self, module_name: str) -> None:
        """Обрабатывает зависимость по имени модуля"""
        if module_name.lower() in {u.lower() for u in self.STANDARD_DELPHI_UNITS}:
            self.logger.debug(f"Skipping standard unit: {module_name}")
            return

        resolved_path = self._resolve_module_path(module_name)
        if resolved_path:
            self.dependencies.add(resolved_path)
            self.logger.info(f"Found module dependency: {resolved_path}")
            if resolved_path.suffix.lower() == ".pas":
                self._analyze_pas_file(resolved_path)
        else:
            self.missing_files.add(module_name)
            self.logger.warning(f"Module not found: {module_name}")

    def _process_file_path(self, file_path: str) -> None:
        """Обрабатывает зависимость по пути к файлу"""
        full_path = self.convert_to_absolute_path(file_path)
        if self.validate_path(full_path):
            self.dependencies.add(full_path)
            self.logger.info(f"Found file dependency: {full_path}")
            if full_path.suffix.lower() == ".pas":
                self._analyze_pas_file(full_path)
        else:
            self.missing_files.add(file_path)
            self.logger.warning(f"File not found: {file_path}")

    def _analyze_conditional_block(self, content: str) -> None:
        """Анализирует блок условной компиляции"""
        # Поиск файловых зависимостей в условном блоке
        file_pattern = r"\b\w+\.(pas|inc|res|rc|dfm)\b"
        for match in re.finditer(file_pattern, content, re.IGNORECASE):
            self._add_dependency(match.group(0))

    def _analyze_uses_dependencies(self, content: str, path: str) -> None:
        """Анализирует секции uses в модуле и обрабатывает зависимости"""
        # Улучшенное регулярное выражение для секций uses
        uses_pattern = r"\buses\s+((?:.*?)+?);"

        # Ищем все секции uses (независимо от регистра)
        for uses_match in re.finditer(uses_pattern, content, re.IGNORECASE | re.DOTALL):
            units_section = uses_match.group(1)
            # Удаляем комментарии и лишние символы
            units_section = re.sub(
                r"\{.*?\}|\/\/.*", "", units_section, flags=re.DOTALL
            )
            units = [u.strip() for u in re.split(r"[\n,]", units_section) if u.strip()]

            self.logger.debug(f"Found units in {Path(path).name}: {units}")
            clean_units = []
            for unit in units:
                if unit:
                    self._add_dependency(unit)
                    clean_units.append(unit)
            self._build_dependency_graph(path, clean_units)

    @property
    def missing_dependencies(self) -> Set[str]:
        """Returns the set of missing dependencies found during analysis"""
        return self.missing_files

    def save_dproj(self, path: str) -> None:
        """Saves the project configuration as a DPROJ file with proper formatting

        Args:
            path: The path where to save the DPROJ file
        """
        self.logger.info("Converting project to DPROJ format...")
        project = self.convert_to_dproj()

        # Convert to string
        rough_string = ET.tostring(project, encoding="utf-8")

        # Pretty print XML
        self.logger.info("Formatting XML output...")
        dom = xml.dom.minidom.parseString(rough_string)
        pretty_xml = dom.toprettyxml(indent="    ", encoding="utf-8")

        # Write to file
        with open(path, "wb") as f:
            f.write(pretty_xml)
        self.logger.info("DPROJ file has been formatted and saved successfully")

    def _print_dependency_tree(self, show_standard_units: bool = True) -> None:
        """Выводит дерево зависимостей проекта с псевдографикой"""

        def log_tree(
            module: str,
            graph: Dict[str, Set[str]],
            prefix: str = "",
            visited: Set[str] = set(),
        ) -> Tuple[int, int]:
            """
            Рекурсивно логирует дерево зависимостей
            Возвращает кортеж: (количество кастомных зависимостей, количество стандартных)
            """
            custom_count = 0
            standard_count = 0

            # Получаем отсортированные зависимости
            deps = sorted(graph.get(module, set()))
            if not deps:
                return 0, 0

            # Проверка циклических зависимостей
            if module in visited:
                self.logger.info(f"{prefix}└── {module} (circular dependency)")
                return 0, 0

            visited.add(module)

            for i, dep in enumerate(deps):
                is_last = i == len(deps) - 1
                connector = "└── " if is_last else "├── "
                next_prefix = prefix + ("    " if is_last else "│   ")

                # Проверяем, является ли зависимость стандартной
                is_standard = dep.lower() in {
                    u.lower() for u in self.STANDARD_DELPHI_UNITS
                }

                if is_standard and not show_standard_units:
                    standard_count += 1
                    continue

                # Логируем зависимость
                self.logger.info(f"{prefix}{connector}{dep}")
                custom_count += 1

                # Рекурсивно обрабатываем вложенные зависимости
                sub_custom, sub_standard = log_tree(
                    dep, graph, next_prefix, visited.copy()
                )
                custom_count += sub_custom
                standard_count += sub_standard

            return custom_count, standard_count

        # Получаем имя корневого модуля из пути проекта
        root_module = Path(self.project_path).stem
        self.logger.info(f"\nProject: {root_module}")

        # Строим дерево
        custom, standard = log_tree(root_module, self.dependency_graph)

        # Выводим итоговую статистику
        self.logger.info(
            f"\nTotal dependencies: {custom + standard} "
            f"({custom} custom, {standard} standard)"
        )

        # Проверяем циклические зависимости
        cycles = self._detect_cycles()
        if cycles:
            self.logger.warning("\nCircular Dependencies Detected:")
            for cycle in cycles:
                self.logger.warning(f"  → {' → '.join(cycle)} →")

    def process(self) -> bool:
        """Основной метод обработки проекта"""
        try:
            self.read_dof()
            self.analyze_dependencies()

            # Check for circular dependencies
            cycles = self._detect_cycles()
            if cycles:
                for cycle in cycles:
                    cycle_str = " -> ".join(cycle)
                    self.logger.warning(f"Circular dependency detected: {cycle_str}")

            return True
        except Exception as e:
            self.logger.error(f"Error processing project: {e}")
            return False
