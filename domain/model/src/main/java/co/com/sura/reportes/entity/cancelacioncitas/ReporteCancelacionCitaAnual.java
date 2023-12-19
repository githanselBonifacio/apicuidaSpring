package co.com.sura.reportes.entity.cancelacioncitas;

import co.com.sura.maestros.entity.Regional;
import lombok.*;
import lombok.experimental.SuperBuilder;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import static co.com.sura.reportes.entity.cancelacioncitas.RegistroCancelacionCita.calcularTotalCantidad;


@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Data
@EqualsAndHashCode(callSuper=false)
@SuperBuilder(toBuilder = true)
public class ReporteCancelacionCitaAnual {
    private Regional regional;
    private ResumenCancelacionCita resumen;
    private List<RegistroCancelacionCitaAnual> reportes;

    public static Mono<ReporteCancelacionCitaAnual> agruparReportes(ReporteCancelacionCitaAnual reporte) {
        return Flux.fromIterable(reporte.getReportes())
           .groupBy(RegistroCancelacionCitaAnual::getMes)
           .flatMap(grouped -> grouped.collectMultimap(RegistroCancelacionCitaAnual::getMes)
           .map(list -> {
                var registros = list.values().stream()
                      .flatMap(Collection::stream)
                      .map(r-> r.getRegistros().get(0))
                      .collect(Collectors.toList());
                 return  new RegistroCancelacionCitaAnual(grouped.key(), calcularTotalCantidad(registros), registros);
                })
              )
              .collectList()
              .map(RegistroCancelacionCitaAnual::ordenarListaByMes)
              .doOnNext(reporte::setReportes)
              .thenReturn(reporte);
    }
    public static Mono<ReporteCancelacionCitaAnual> calcularResumen(ReporteCancelacionCitaAnual reporte){
        return Flux.fromIterable((reporte.getReportes()))
                .flatMap(registro -> Flux.fromIterable(registro.getRegistros()))
                .reduce(ResumenCancelacionCita.builder()
                    .registrosCancelacion(new ArrayList<>())
                    .build(),
                    (resumen, registro) -> {
                          resumen.setTotalCancelaciones(resumen.getTotalCancelaciones() + registro.getCantidad());
                          resumen.getRegistrosCancelacion().stream()
                          .filter(r -> r.getDescripcion().equals(registro.getDescripcion())).findFirst()
                          .ifPresentOrElse(er -> er.setCantidad(er.getCantidad() + registro.getCantidad()),
                                  () -> resumen.getRegistrosCancelacion().add(registro)
                            );
                    return resumen;
                })
                .doOnNext(reporte::setResumen)
                .thenReturn(reporte);

    }
}
