#!/usr/bin/env python3
"""
Gera planilha Excel (.xlsx) profissional contendo as tabelas do Discurso Narrativo:
1. Kruskal-Wallis (IP, InfL, CompT)
2. Post-Hoc Wilcoxon ajustado por Holm (comparações par a par)
3. Subteste Inferência (Tabela de contingência e Teste Exato de Fisher)
4. Estatística Descritiva Estratificada por Escolaridade
"""

import zipfile
import io
import os
import math

def col_letter(col_idx):
    res = ""
    while col_idx > 0:
        col_idx, rem = divmod(col_idx - 1, 26)
        res = chr(65 + rem) + res
    return res

def build_xlsx(sheets, filepath):
    zbuf = io.BytesIO()
    with zipfile.ZipFile(zbuf, "w", zipfile.ZIP_DEFLATED) as z:
        # 1. [Content_Types].xml
        content_types = [
            '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
            '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
            '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
            '<Default Extension="xml" ContentType="application/xml"/>',
            '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>',
            '<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>'
        ]
        for i in range(1, len(sheets) + 1):
            content_types.append(f'<Override PartName="/xl/worksheets/sheet{i}.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>')
        content_types.append('</Types>')
        z.writestr("[Content_Types].xml", "".join(content_types))

        # 2. _rels/.rels
        rels = [
            '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
            '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
            '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>',
            '</Relationships>'
        ]
        z.writestr("_rels/.rels", "".join(rels))

        # 3. xl/workbook.xml
        wb_xml = [
            '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
            '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
            '<sheets>'
        ]
        for i, (name, _, _) in enumerate(sheets, 1):
            wb_xml.append(f'<sheet name="{name}" sheetId="{i}" r:id="rId{i}"/>')
        wb_xml.append('</sheets></workbook>')
        z.writestr("xl/workbook.xml", "".join(wb_xml))

        # 4. xl/_rels/workbook.xml.rels
        wb_rels = [
            '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
            '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
        ]
        for i in range(1, len(sheets) + 1):
            wb_rels.append(f'<Relationship Id="rId{i}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet{i}.xml"/>')
        wb_rels.append(f'<Relationship Id="rId{len(sheets)+1}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>')
        wb_rels.append('</Relationships>')
        z.writestr("xl/_rels/workbook.xml.rels", "".join(wb_rels))

        # 5. xl/styles.xml
        styles_xml = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">
  <fonts count="2">
    <font><name val="Calibri"/><family val="2"/><sz val="11"/></font>
    <font><b/><color rgb="FFFFFFFF"/><name val="Calibri"/><family val="2"/><sz val="11"/></font>
  </fonts>
  <fills count="3">
    <fill><patternFill patternType="none"/></fill>
    <fill><patternFill patternType="gray125"/></fill>
    <fill><patternFill patternType="solid"><fgColor rgb="FF1F4E78"/></patternFill></fill>
  </fills>
  <borders count="2">
    <border><left/><right/><top/><bottom/><diagonal/></border>
    <border>
      <left style="thin"><color rgb="FFD9D9D9"/></left>
      <right style="thin"><color rgb="FFD9D9D9"/></right>
      <top style="thin"><color rgb="FFD9D9D9"/></top>
      <bottom style="thin"><color rgb="FFD9D9D9"/></bottom>
    </border>
  </borders>
  <cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>
  <cellXfs count="4">
    <xf numFmtId="0" fontId="0" fillId="0" borderId="1" xfId="0" applyBorder="1"/>
    <!-- Style 1: Header (Navy Blue, Bold White, Center) -->
    <xf numFmtId="0" fontId="1" fillId="2" borderId="1" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1">
      <alignment horizontal="center" vertical="center" wrapText="1"/>
    </xf>
    <!-- Style 2: Data Center -->
    <xf numFmtId="0" fontId="0" fillId="0" borderId="1" xfId="0" applyBorder="1" applyAlignment="1">
      <alignment horizontal="center" vertical="center"/>
    </xf>
    <!-- Style 3: Data Left -->
    <xf numFmtId="0" fontId="0" fillId="0" borderId="1" xfId="0" applyBorder="1" applyAlignment="1">
      <alignment horizontal="left" vertical="center"/>
    </xf>
  </cellXfs>
</styleSheet>"""
        z.writestr("xl/styles.xml", styles_xml)

        # 6. Worksheets
        for i, (name, headers, rows) in enumerate(sheets, 1):
            sheet_lines = [
                '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
                '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
                '<sheetViews><sheetView tabSelected="1" workbookViewId="0"/></sheetViews>',
                '<sheetFormatPr defaultRowHeight="20"/>'
            ]

            # Calculate col widths
            col_widths = []
            for c_idx, h in enumerate(headers):
                max_l = len(str(h))
                for r in rows:
                    if c_idx < len(r):
                        val_str = str(r[c_idx])
                        max_l = max(max_l, len(val_str))
                col_widths.append(max(max_l + 5, 14))

            sheet_lines.append('<cols>')
            for c_idx, w in enumerate(col_widths, 1):
                sheet_lines.append(f'<col min="{c_idx}" max="{c_idx}" width="{w}" customWidth="1"/>')
            sheet_lines.append('</cols>')

            sheet_lines.append('<sheetData>')
            # Header row
            sheet_lines.append('<row r="1" ht="26" customHeight="1">')
            for c_idx, h in enumerate(headers, 1):
                ref = f"{col_letter(c_idx)}1"
                h_clean = str(h).replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
                sheet_lines.append(f'<c r="{ref}" s="1" t="inlineStr"><is><t>{h_clean}</t></is></c>')
            sheet_lines.append('</row>')

            # Data rows
            for r_idx, row in enumerate(rows, 2):
                sheet_lines.append(f'<row r="{r_idx}" ht="20">')
                for c_idx, val in enumerate(row, 1):
                    ref = f"{col_letter(c_idx)}{r_idx}"
                    if isinstance(val, (int, float)) and not math.isnan(val):
                        # Numeric cell
                        sheet_lines.append(f'<c r="{ref}" s="2"><v>{val}</v></c>')
                    else:
                        val_str = "" if val is None or (isinstance(val, float) and math.isnan(val)) else str(val)
                        val_str = val_str.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
                        # If string is longer than 20 chars or contains 'vs' / text name, align left (s="3") else center (s="2")
                        style_idx = "3" if len(val_str) > 20 or any(c in val_str for c in ["Ideias", "Informa", "Compreen"]) else "2"
                        sheet_lines.append(f'<c r="{ref}" s="{style_idx}" t="inlineStr"><is><t>{val_str}</t></is></c>')
                sheet_lines.append('</row>')

            sheet_lines.append('</sheetData></worksheet>')
            z.writestr(f"xl/worksheets/sheet{i}.xml", "".join(sheet_lines))

    # Save to disk
    os.makedirs(os.path.dirname(os.path.abspath(filepath)), exist_ok=True)
    with open(filepath, "wb") as f:
        f.write(zbuf.getvalue())
    print(f"Planilha gerada com sucesso: {filepath} ({len(zbuf.getvalue())} bytes)")

def main():
    # 1. Tabela Kruskal-Wallis: Discurso Narrativo
    headers_kw = ["Subteste", "Tarefa", "Estatística H", "gl", "p-valor", "Resultado"]
    rows_kw = [
        ["IP", "Ideias Principais", 5.0041, 2, "0.08192", "Não significativo"],
        ["InfL", "Informações Lembradas", 5.6284, 2, "0.05995", "Não significativo"],
        ["CompT", "Compreensão do Texto", 4.7830, 2, "0.09149", "Não significativo"]
    ]

    # 2. Tabela Post-Hoc Wilcoxon com ajuste de Holm: Discurso Narrativo
    headers_ph = ["Subteste", "Comparação", "Média G1", "Média G2", "Diferença", "p-valor (Holm)", "Resultado"]
    rows_ph = [
        ["IP", "5-8 anos vs 9-11 anos", 4.30, 6.29, -1.98, "0.15748", "Não significativo"],
        ["IP", "5-8 anos vs 12+ anos", 4.30, 7.17, -2.86, "0.24370", "Não significativo"],
        ["IP", "9-11 anos vs 12+ anos", 6.29, 7.17, -0.88, "0.64174", "Não significativo"],
        ["InfL", "5-8 anos vs 9-11 anos", 6.39, 8.93, -2.54, "0.14834", "Não significativo"],
        ["InfL", "5-8 anos vs 12+ anos", 6.39, 11.17, -4.78, "0.13492", "Não significativo"],
        ["InfL", "9-11 anos vs 12+ anos", 8.93, 11.17, -2.24, "0.61285", "Não significativo"],
        ["CompT", "5-8 anos vs 9-11 anos", 5.70, 8.00, -2.30, "0.23014", "Não significativo"],
        ["CompT", "5-8 anos vs 12+ anos", 5.70, 9.00, -3.30, "0.23014", "Não significativo"],
        ["CompT", "9-11 anos vs 12+ anos", 8.00, 9.00, -1.00, "0.58168", "Não significativo"]
    ]

    # 3. Tabela Subteste Inferência (Contingência e Teste Exato de Fisher)
    headers_infer = ["Escolaridade", "Erro (0)", "Acerto (1)", "Total", "Taxa de Acerto (%)", "p-valor (Fisher)", "Resultado"]
    rows_infer = [
        ["5-8 anos", 15, 7, 22, "31.82%", "0.74913", "Não significativo"],
        ["9-11 anos", 9, 5, 14, "35.71%", "", ""],
        ["12+ anos", 3, 3, 6, "50.00%", "", ""]
    ]

    # 4. Tabela Estatística Descritiva Estratificada por Escolaridade
    headers_desc = ["Subteste", "Tarefa", "Escolaridade", "N", "Média", "Desvio-Padrão", "Mediana", "Q1", "Q3", "Ponto de corte"]
    rows_desc = [
        # IP
        ["IP", "Ideias Principais", "5-8 anos", 23, 4.30, 2.96, 4.00, 2.00, 6.50, 4.00],
        ["IP", "Ideias Principais", "9-11 anos", 14, 6.29, 2.84, 6.50, 4.25, 9.00, 3.00],
        ["IP", "Ideias Principais", "12+ anos", 6, 7.17, 4.17, 7.00, 5.25, 9.50, 6.00],
        # InfL
        ["InfL", "Informações Lembradas", "5-8 anos", 23, 6.39, 3.63, 7.00, 4.00, 9.00, 4.00],
        ["InfL", "Informações Lembradas", "9-11 anos", 14, 8.93, 3.99, 10.50, 7.00, 12.00, 5.00],
        ["InfL", "Informações Lembradas", "12+ anos", 6, 11.17, 6.24, 10.50, 9.25, 13.25, 10.00],
        # CompT
        ["CompT", "Compreensão do Texto", "5-8 anos", 23, 5.70, 3.87, 5.00, 3.00, 9.00, 6.00],
        ["CompT", "Compreensão do Texto", "9-11 anos", 14, 8.00, 3.59, 9.50, 4.00, 10.00, 8.00],
        ["CompT", "Compreensão do Texto", "12+ anos", 6, 9.00, 4.10, 9.50, 6.50, 11.75, 8.00],
        # Infer
        ["Infer", "Inferência", "5-8 anos", 22, 0.32, 0.48, 0.00, 0.00, 1.00, "-"],
        ["Infer", "Inferência", "9-11 anos", 14, 0.36, 0.50, 0.00, 0.00, 1.00, "-"],
        ["Infer", "Inferência", "12+ anos", 6, 0.50, 0.55, 0.50, 0.00, 1.00, "-"]
    ]

    sheets = [
        ("KW_Narrativo", headers_kw, rows_kw),
        ("PostHoc_Narrativo", headers_ph, rows_ph),
        ("Inferencia_Fisher", headers_infer, rows_infer),
        ("Descritiva_Narrativo", headers_desc, rows_desc)
    ]

    # Gerar a planilha em report/ e data/
    out_paths = [
        "report/tabelas_discurso_narrativo.xlsx",
        "data/tabelas_discurso_narrativo.xlsx",
        "tabelas_discurso_narrativo.xlsx"
    ]
    for p in out_paths:
        build_xlsx(sheets, p)

if __name__ == "__main__":
    main()

