package co.com.sura.postgres.repository.reportes.adapter;

import co.com.sura.entity.reportes.ItemReporteAnual;
import co.com.sura.entity.reportes.ItemReporteMensual;
import co.com.sura.postgres.Converter;
import io.r2dbc.spi.Row;

public class ConvertReporte extends Converter {

    public static ItemReporteAnual   buildItemReporteAnualFromRow(Row row){
        return ItemReporteAnual
                .builder()
                .mes(row.get("mes", String.class))
                .capacidadPromedio(row.get("capacidadPromedio", Double.class))
                .totalHorasAtencionesCompletadas(row.get("totalHorasAtencionesCompletadas", Double.class))
                .totalCitasCompletadas(row.get("totalCitasCompletadas", Integer.class))
                .totalCitasCanceladas(row.get("totalCitasCanceladas", Integer.class))
                .cumplimientoCitasPromedio(row.get("cumplimientoCitasPromedio", Double.class))
                .build();
    }
    public static ItemReporteMensual   buildItemReporteMensualFromRow(Row row){
        return ItemReporteMensual
                .builder()
                .dia(row.get("dia", String.class))
                .capacidadPromedio(row.get("capacidadPromedio", Double.class))
                .totalHorasAtencionesCompletadas(row.get("totalHorasAtencionesCompletadas", Double.class))
                .totalCitasCompletadas(row.get("totalCitasCompletadas", Integer.class))
                .totalCitasCanceladas(row.get("totalCitasCanceladas", Integer.class))
                .cumplimientoCitasPromedio(row.get("cumplimientoCitasPromedio", Double.class))
                .build();
    }
}
