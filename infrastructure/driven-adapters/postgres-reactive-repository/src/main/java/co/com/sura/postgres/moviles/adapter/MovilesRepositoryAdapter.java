package co.com.sura.postgres.moviles.adapter;

import co.com.sura.moviles.entity.Desplazamiento;
import co.com.sura.moviles.gateway.MovilRepository;
import co.com.sura.postgres.Converter;
import co.com.sura.postgres.moviles.data.DesplazamientoRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;

import java.time.LocalDate;

@Repository
public class MovilesRepositoryAdapter implements MovilRepository {

    @Autowired
    private DesplazamientoRepository desplazamientoRepository;

    @Override
    public Flux<Desplazamiento> consultarDesplazamientoRegional(LocalDate fechaProgramada, Integer idHorarioTurno, String idRegional) {
        return desplazamientoRepository.findByFechaProgramada(fechaProgramada,idRegional,idHorarioTurno)
                .map(desplazamientoData -> Converter.converToEntity(desplazamientoData, Desplazamiento.class));
    }
}
