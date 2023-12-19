package co.com.sura.reportes.entity.turnos;

import lombok.*;
import lombok.experimental.SuperBuilder;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;


@Getter
@Setter
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper=false)
@ToString
@SuperBuilder(toBuilder = true)
public class ReporteTurnoMensual extends ReporteTurno {
    private List<ItemReporteMensual> reportes;

    public static Mono<ReporteTurnoMensual> calcularResumen(ReporteTurnoMensual reporte) {
        int sizeReporte = reporte.getReportes().size();
        return Flux.fromIterable(reporte.getReportes())
                .reduce(ResumenReporteTurno.builder().build(), (resumen, item) -> {
                    resumen.setCapacidad(resumen.getCapacidad() + item.getCapacidadPromedio());
                    resumen.setTotalHoras(resumen.getTotalHoras() + item.getTotalHorasAtencionesCompletadas());
                    resumen.setCitasCompletadas(resumen.getCitasCompletadas() + item.getTotalCitasCompletadas());
                    resumen.setCitasCanceladas(resumen.getCitasCanceladas() + item.getTotalCitasCanceladas());
                    resumen.setTotalRemisiones(resumen.getTotalRemisiones() + item.getTotalRemisiones());
                    resumen.setTotalNovedades(resumen.getTotalNovedades() + item.getTotalNovedades());
                    resumen.setCumplimiento(resumen.getCumplimiento() + item.getCumplimientoCitasPromedio());
                    return resumen;
                })
                .map(resumen -> {
                    if(sizeReporte>0){
                        resumen.setCapacidad(resumen.getCapacidad() / sizeReporte);
                        resumen.setCumplimiento(resumen.getCumplimiento() / sizeReporte);
                    }
                    return resumen;
                })
                .doOnNext(reporte::setResumen)
                .thenReturn(reporte);
    }
}
