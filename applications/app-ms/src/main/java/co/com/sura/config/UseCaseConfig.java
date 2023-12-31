package co.com.sura.config;

import co.com.sura.agenda.AgendaUseCase;
import co.com.sura.entity.agenda.AgendaRepository;
import co.com.sura.entity.maestro.MaestroRepository;
import co.com.sura.entity.moviles.MovilRepository;
import co.com.sura.entity.remision.RemisionCrudRepository;
import co.com.sura.maestro.CrudMaestroUseCase;
import co.com.sura.moviles.MovilesUseCase;
import co.com.sura.remision.RemisionUseCase;
import co.com.sura.services.mapbox.MapboxService;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@RequiredArgsConstructor
@Configuration
public class UseCaseConfig {

    @Bean
    public CrudMaestroUseCase crudMaestroUseCase (MaestroRepository maestroRepository){
        return new CrudMaestroUseCase(maestroRepository);
    }

    @Bean
    public AgendaUseCase agendaUseCase (AgendaRepository agendaRepository){
        return new AgendaUseCase(agendaRepository);
    }

    @Bean
    public RemisionUseCase remisionUseCase (RemisionCrudRepository remisionCrudRepository){
        return new RemisionUseCase(remisionCrudRepository);
    }

    @Bean
    public MovilesUseCase movilesUseCase (MovilRepository movilRepository){
        return new MovilesUseCase(movilRepository);
    }


}
