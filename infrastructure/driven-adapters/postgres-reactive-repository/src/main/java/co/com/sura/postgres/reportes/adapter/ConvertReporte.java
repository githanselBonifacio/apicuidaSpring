package co.com.sura.postgres.reportes.adapter;

import co.com.sura.postgres.maestros.data.HorarioTurnoData;
import co.com.sura.postgres.maestros.data.RegionalData;
import co.com.sura.reportes.entity.cancelacioncitas.RegistroCancelacionCita;
import co.com.sura.reportes.entity.turnos.ItemReporteAnual;
import co.com.sura.reportes.entity.turnos.ItemReporteMensual;
import co.com.sura.reportes.entity.cancelacioncitas.RegistroCancelacionCitaAnual;
import co.com.sura.reportes.entity.cancelacioncitas.RegistroCancelacionCitaMensual;
import co.com.sura.postgres.Converter;
import co.com.sura.postgres.agenda.data.CitaData;
import co.com.sura.postgres.reportes.data.ReporteTurnoData;
import io.r2dbc.spi.Row;
import org.javatuples.Pair;
import reactor.util.function.Tuple5;
import java.time.LocalDate;
import java.util.Collections;
import java.util.List;


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
                .registros(Collections.singletonList(RegistroCancelacionCita
                        .builder()
                        .descripcion(row.get("descripcion", String.class))
                        .cantidad(row.get("cantidadCancelaciones", Integer.class))
                        .build()))
                .build();
    }
    public static RegistroCancelacionCitaMensual buildRegistroCancelacionCitaMensualFromRow(Row row){
        return RegistroCancelacionCitaMensual
                .builder()
                .dia(row.get("dia", String.class))
                .registros(Collections.singletonList(RegistroCancelacionCita
                        .builder()
                        .descripcion(row.get("descripcion", String.class))
                        .cantidad(row.get("cantidadCancelaciones", Integer.class))
                        .build()))
                .build();
    }

    public static ReporteTurnoData buildReporteTurnoData(
            LocalDate fechaTurno,
            Pair<RegionalData, HorarioTurnoData> pair,
            Tuple5<List<CitaData>, Integer, Integer, Integer, Integer> tuple){

        var horasAsignadasCitas = CitaData.duracionTotalCitas(tuple.getT1());
        var horasCompletadasCitas = CitaData.horasCompletadasCitas(tuple.getT1());
        var citasCompletadas = CitaData.citasCompletadas(tuple.getT1());
        var citasCanceladas =  CitaData.citasCanceladas(tuple.getT1());

        Double capacidad = CitaData.capacidadTurno((horasAsignadasCitas + tuple.getT5()),
                (tuple.getT4().doubleValue() * pair.getValue1().getDuracionHoras()));

        return ReporteTurnoData.builder()
                .fechaTurno(fechaTurno)
                .idRegional(pair.getValue0().getId())
                .idHorarioTurno(pair.getValue1().getId())
                .citasAsignadas(tuple.getT1().size())
                .citasCompletadas(citasCompletadas)
                .citasCanceladas(citasCanceladas)
                .totalRemisiones(tuple.getT2())
                .totalNovedades(tuple.getT3())
                .totalProfesionales(tuple.getT4())
                .horasProfesionales((double)tuple.getT4()*pair.getValue1().getDuracionHoras())
                .horasAsignadas(horasAsignadasCitas)
                .horasCompletadas(horasCompletadasCitas)
                .capacidadActual(capacidad)
                .build();
    }
}
