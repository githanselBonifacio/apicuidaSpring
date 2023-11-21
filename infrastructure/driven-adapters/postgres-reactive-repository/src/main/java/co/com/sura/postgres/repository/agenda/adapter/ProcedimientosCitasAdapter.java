package co.com.sura.postgres.repository.agenda.adapter;

import co.com.sura.entity.agenda.ProcedimientosCitaRepository;
import co.com.sura.entity.remision.Canalizacion;
import co.com.sura.entity.remision.Curacion;
import co.com.sura.entity.remision.Fototerapia;
import co.com.sura.entity.remision.Procedimientos;
import co.com.sura.entity.remision.Secrecion;
import co.com.sura.entity.remision.Sondaje;
import co.com.sura.entity.remision.SoporteNutricional;
import co.com.sura.entity.remision.TomaMuestra;
import co.com.sura.postgres.repository.remision.data.CanalizacionRepository;
import co.com.sura.postgres.repository.remision.data.CuracionRepository;
import co.com.sura.postgres.repository.remision.data.FototerapiaRepository;
import co.com.sura.postgres.repository.remision.data.SecrecionRepository;
import co.com.sura.postgres.repository.remision.data.SondajeRepository;
import co.com.sura.postgres.repository.remision.data.SoporteNutricionalRepository;
import co.com.sura.postgres.repository.remision.data.TomaMuestraRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Repository
public class ProcedimientosCitasAdapter implements ProcedimientosCitaRepository {

    private final CuracionRepository curacionRepository;
    private final CanalizacionRepository canalizacionRepository;
    private final FototerapiaRepository fototerapiaRepository;
    private final SecrecionRepository secrecionRepository;
    private final SondajeRepository sondajeRepository;
    private final SoporteNutricionalRepository soporteNutricionalRepository;
    private final TomaMuestraRepository tomaMuestraRepository;

    @Autowired
    public ProcedimientosCitasAdapter(CuracionRepository curacionRepository,
                                      CanalizacionRepository canalizacionRepository,
                                      FototerapiaRepository fototerapiaRepository,
                                      SecrecionRepository secrecionRepository,
                                      SondajeRepository sondajeRepository,
                                      SoporteNutricionalRepository soporteNutricionalRepository,
                                      TomaMuestraRepository tomaMuestraRepository) {
        this.curacionRepository = curacionRepository;
        this.canalizacionRepository = canalizacionRepository;
        this.fototerapiaRepository = fototerapiaRepository;
        this.secrecionRepository = secrecionRepository;
        this.sondajeRepository = sondajeRepository;
        this.soporteNutricionalRepository = soporteNutricionalRepository;
        this.tomaMuestraRepository = tomaMuestraRepository;
    }

    @Override
    public Mono<Procedimientos> consultarProcedimientosByIdCita(String idCita) {
        return Mono.zip(
                        consultarCanalizacionesByCitas(idCita).collectList(),
                        consultarCuracionesByCitas(idCita).collectList(),
                        consultarFototerapiasByCitas(idCita).collectList(),
                        consultarSecrecionesByCitas(idCita).collectList(),
                        consultarSondajesByCitas(idCita).collectList(),
                        consultarTomaMuestrasByCitas(idCita).collectList(),
                        consultarSoporteNutricionalesByCitas(idCita).collectList())
                .map(tuple->Procedimientos.builder()
                        .canalizaciones(tuple.getT1())
                        .curaciones(tuple.getT2())
                        .fototerapias(tuple.getT3())
                        .secreciones(tuple.getT4())
                        .sondajes(tuple.getT5())
                        .tomaMuestras(tuple.getT6())
                        .soporteNutricionales(tuple.getT7())
                        .build()

                );
    }

    @Override
    public Flux<Curacion> consultarCuracionesByCitas(String idCita) {
        return curacionRepository.findByIdCita(idCita)
                .map(ConverterAgenda :: convertToCuracion);
    }

    @Override
    public Flux<Canalizacion> consultarCanalizacionesByCitas(String idCita) {
        return canalizacionRepository.findByIdCita(idCita)
                .map(ConverterAgenda ::convertToCanalizacion);
    }

    @Override
    public Flux<Fototerapia> consultarFototerapiasByCitas(String idCita) {
        return fototerapiaRepository.findByIdCita(idCita)
                .map(ConverterAgenda :: convertToFototerapia);
    }

    @Override
    public Flux<Secrecion> consultarSecrecionesByCitas(String idCita) {
        return secrecionRepository.findByIdCita(idCita)
                .map(ConverterAgenda :: convertToSecrecionData);
    }

    @Override
    public Flux<Sondaje> consultarSondajesByCitas(String idCita) {
        return sondajeRepository.findByIdCita(idCita)
                .map(ConverterAgenda :: convertToSondajeData);
    }

    @Override
    public Flux<TomaMuestra> consultarTomaMuestrasByCitas(String idCita) {
        return tomaMuestraRepository.findByIdCita(idCita)
                .map(ConverterAgenda :: convertToTomaMuestraData);
    }

    @Override
    public Flux<SoporteNutricional> consultarSoporteNutricionalesByCitas(String idCita) {
        return soporteNutricionalRepository.findByIdCita(idCita)
                .map(ConverterAgenda::convertToSoporteNutricionalData);
    }
}
