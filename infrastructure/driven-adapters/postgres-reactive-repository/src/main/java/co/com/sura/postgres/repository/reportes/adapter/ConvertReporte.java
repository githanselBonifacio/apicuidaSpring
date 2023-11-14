package co.com.sura.postgres.repository.reportes.adapter;

import co.com.sura.entity.reportes.turnos.ItemReporteAnual;
import co.com.sura.entity.reportes.turnos.ItemReporteMensual;
import co.com.sura.entity.reportes.cancelacioncitas.RegistroCancelacionCitaAnual;
import co.com.sura.entity.reportes.cancelacioncitas.RegistroCancelacionCitaMensual;
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
                .totalRemisiones(row.get("totalRemisiones", Integer.class))
                .totalNovedades(row.get("totalNovedades", Integer.class))
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
                .totalRemisiones(row.get("totalRemisiones", Integer.class))
                .totalNovedades(row.get("totalNovedades", Integer.class))
                .cumplimientoCitasPromedio(row.get("cumplimientoCitasPromedio", Double.class))
                .build();
    }
    public static RegistroCancelacionCitaAnual buildRegistroCancelacionCitaAnualFromRow(Row row){
        return RegistroCancelacionCitaAnual
                .builder()
                .mes(row.get("mes", String.class))
                .descripcion(row.get("descripcion", String.class))
                .cantidad(row.get("cantidadCancelaciones", Integer.class))
                .build();
    }
    public static RegistroCancelacionCitaMensual buildRegistroCancelacionCitaMensualFromRow(Row row){
        return RegistroCancelacionCitaMensual
                .builder()
                .dia(row.get("dia", String.class))
                .descripcion(row.get("descripcion", String.class))
                .cantidad(row.get("cantidadCancelaciones", Integer.class))
                .build();
    }
}
