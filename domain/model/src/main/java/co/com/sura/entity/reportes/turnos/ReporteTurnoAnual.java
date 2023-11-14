package co.com.sura.entity.reportes.turnos;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@SuperBuilder(toBuilder = true)
public class ReporteTurnoAnual extends ReporteTurno {
    private List<ItemReporteAnual> reportes;

    public static Mono<ReporteTurnoAnual> setNombreMes(ReporteTurnoAnual reporte){
        return Flux.fromIterable(reporte.getReportes())
                .map(ItemReporteAnual::converNumeroToNombreMes)
                .collectList()
                .thenReturn(reporte);
    }
  public static Mono<ReporteTurnoAnual> calcularResumen(ReporteTurnoAnual reporte) {
        int sizeReporte = reporte.getReportes().size();
        return Flux.fromIterable(reporte.getReportes())
                .reduce(ResumenReporteTurno.builder().build(), (resumen, item) -> {
                    resumen.setCapacidad(resumen.getCapacidad() + item.getCapacidadPromedio());
                    resumen.setTotalHoras(resumen.getTotalHoras() + item.getTotalHorasAtencionesCompletadas());
                    resumen.setCitasCompletadas(resumen.getCitasCompletadas() + item.getTotalCitasCompletadas());
                    resumen.setCitasCanceladas(resumen.getCitasCanceladas() + item.getTotalCitasCanceladas());
                    resumen.setTotalRemisiones(resumen.getTotalRemisiones() + item.getTotalRemisiones());
                    resumen.setTotalNovedades(resumen.getTotalNovedades() + item.getTotalNovedades());
                    resumen.setCumplimineto(resumen.getCumplimineto() + item.getCumplimientoCitasPromedio());
                    return resumen;
                })
                .map(resumen -> {
                    resumen.setCapacidad(resumen.getCapacidad() / sizeReporte);
                    resumen.setCumplimineto(resumen.getCumplimineto() / sizeReporte);
                    return resumen;
                })
                .doOnNext(reporte::setResumen)
                .thenReturn(reporte);

    }
}
