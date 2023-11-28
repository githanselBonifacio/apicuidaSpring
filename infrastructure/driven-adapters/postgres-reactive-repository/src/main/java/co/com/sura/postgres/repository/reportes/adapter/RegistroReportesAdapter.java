package co.com.sura.postgres.repository.reportes.adapter;

import co.com.sura.postgres.repository.remision.data.RegistroHistorialRepository;
import co.com.sura.postgres.repository.remision.data.RemisionRepository;
import co.com.sura.postgres.repository.agenda.data.CitaData;
import co.com.sura.postgres.repository.agenda.data.CitaRepository;
import co.com.sura.postgres.repository.personal.data.TurnoProfesionalesRepository;
import co.com.sura.postgres.repository.maestros.data.HorarioTurnoData;
import co.com.sura.postgres.repository.maestros.data.HorarioTurnoRepository;
import co.com.sura.postgres.repository.maestros.data.RegionalesData;
import co.com.sura.postgres.repository.maestros.data.RegionalesRepository;
import co.com.sura.postgres.repository.moviles.data.DesplazamientoData;
import co.com.sura.postgres.repository.moviles.data.DesplazamientoRepository;
import co.com.sura.postgres.repository.reportes.data.ReporteTurnoRepository;
import org.javatuples.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.util.function.Tuple5;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.List;

@Component
@EnableScheduling
public class RegistroReportesAdapter {
    private final RegionalesRepository regionalesRepository;
    private final RemisionRepository remisionRepository;
    private final CitaRepository citaRepository;
    private final DesplazamientoRepository desplazamientoRepository;
    private final TurnoProfesionalesRepository turnoProfesionalesRepository;
    private final HorarioTurnoRepository horarioTurnoRepository;
    private final RegistroHistorialRepository registroHistorialRepository;
    private final ReporteTurnoRepository reportesTurnoRepository;

    @Autowired
    public RegistroReportesAdapter(RegionalesRepository regionalesRepository, RemisionRepository remisionRepository,
                                   CitaRepository citaRepository, DesplazamientoRepository desplazamientoRepository,
                                   TurnoProfesionalesRepository turnoProfesionalesRepository,
                                   HorarioTurnoRepository horarioTurnoRepository,
                                   RegistroHistorialRepository registroHistorialRepository,
                                   ReporteTurnoRepository reportesTurnoRepository) {

        this.regionalesRepository = regionalesRepository;
        this.remisionRepository = remisionRepository;
        this.citaRepository = citaRepository;
        this.desplazamientoRepository = desplazamientoRepository;
        this.turnoProfesionalesRepository = turnoProfesionalesRepository;
        this.horarioTurnoRepository = horarioTurnoRepository;
        this.registroHistorialRepository = registroHistorialRepository;
        this.reportesTurnoRepository = reportesTurnoRepository;
    }

    @Scheduled(cron = "0 0 1 * * *",  zone = "America/Bogota")
    public void actualizarReporteTurno() {
        var zone = ZoneId.of("America/Bogota");
        var fecha = LocalDate.now(zone).minusDays(1);
        this.insertarRegistroReporteTurno(fecha);
    }

    public void insertarRegistroReporteTurno(LocalDate fechaTurno){
        this.reportesTurnoRepository.deleteByFechaturno(fechaTurno)
                .thenMany(
                        regionalesRepository.findAll()
                                .flatMap(regionalesData -> horarioTurnoRepository.findAll()
                                        .filter(HorarioTurnoData::getEsHorarioBase)

                                        .map(horarioTurnoData->  Pair.with(regionalesData,horarioTurnoData)))

                                .flatMap(pair-> consultarRegistroData(fechaTurno,pair)
                                        .map(tuple-> ConvertReporte.buildReporteTurnoData(fechaTurno, pair, tuple)))

                                .flatMap(this.reportesTurnoRepository::save))
                .subscribe();
    }
    public Flux<Tuple5<List<CitaData>, Integer, Integer, Integer, Integer>> consultarRegistroData(
            LocalDate fechaTurno, Pair<RegionalesData,HorarioTurnoData> pair){
      return Flux.zip(
              citaRepository.findAllByFechaTurnoRegional(fechaTurno, pair.getValue0().getId(),pair.getValue1().getId())
                        .collectList(),
              remisionRepository.countAllByFechaAdmisionIdRegional(fechaTurno,pair.getValue0().getId()),

              registroHistorialRepository.countByFechaNovedadRegional(fechaTurno, pair.getValue0().getId()),

              turnoProfesionalesRepository
                        .countByFechaTurno(fechaTurno, pair.getValue0().getId(),pair.getValue1().getId()),

              desplazamientoRepository
                        .findByFechaProgramada(fechaTurno, pair.getValue0().getId(),pair.getValue1().getId())
                        .transform(DesplazamientoData::calcularHorasTotalesDesplazamientoTurno));
    }
}