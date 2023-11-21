package co.com.sura.entity.reportes.cancelacioncitas;

import co.com.sura.entity.maestro.Regional;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import static co.com.sura.entity.reportes.cancelacioncitas.RegistroCancelacionCita.calcularTotalCantidad;

@Getter
@Setter
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class ReporteCancelacionCitaMensual{
    private Regional regional;
    private ResumenCancelacionCita resumen;
    private List<RegistroCancelacionCitaMensual> reportes;

    public static Mono<ReporteCancelacionCitaMensual> agruparReportes(ReporteCancelacionCitaMensual reporte) {
        return Flux.fromIterable(reporte.getReportes())
            .groupBy(RegistroCancelacionCitaMensual::getDia)
            .flatMap(grouped -> grouped.collectMultimap(RegistroCancelacionCitaMensual::getDia)
              .map(list -> {
                 var registros = list.values().stream()
                      .flatMap(Collection::stream)
                      .map(r-> r.getRegistros().get(0))
                      .collect(Collectors.toList());
                 return  new RegistroCancelacionCitaMensual(grouped.key(), calcularTotalCantidad(registros), registros);
                   })
             )
            .collectList()
            .doOnNext(reporte::setReportes)
            .thenReturn(reporte);
    }

    public static Mono<ReporteCancelacionCitaMensual> calcularResumen(ReporteCancelacionCitaMensual reporte) {
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
    @Override
    public boolean equals(Object o) {
        return super.equals(o);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }
}
